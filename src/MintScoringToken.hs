{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module MintScoringToken
  (
    buildMintingContract,
    policy,
    tokenSymbol
  )
where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import qualified Ledger.Typed.Scripts            as Scripts
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified Plutus.V2.Ledger.Contexts       as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                as P hiding ((.))
import           Prelude                         (Show(..))
import           Plutus.Script.Utils.V2.Scripts  (scriptCurrencySymbol)
import           GeneralParams
import           Utility

data RedeemerParams = RedeemerParams
  {
    {-
      + Factor 0: address balance
      + Factor 1: staking reward
      + Factor 2: payment frequency per month (numbers of transaction history)
      + Factor 3: monthly payment (total sent, total received)
    -} 
    pointsOfFactors :: [Integer],

    {-
      + Weight 0: weight of address balance (25%)
      + Weight 1: weight of staking reward (25%)
      + Weight 2: weight of payment frequency per month (25%)
      + Weight 3: weight of monthly payment (25%)
    -}     
    weights :: [Integer]
  }
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('RedeemerParams,0)]

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: MintParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkTokenPolicy mParams rParams scriptContext =
    traceIfFalse "[Plutus Error]: you're not the operator to mint the Scoring Token"
      ownOperatorTokenInInput &&

    traceIfFalse "[Plutus Error]: minted amount must be one at a time"
      checkMintedAmount &&

    traceIfFalse "[Plutus Error]: your score is not good enough to receive the Scoring Token"
      checkMinScoreToMintScoringToken &&

    traceIfFalse "[Plutus Error]: output datum is not correct"
      (checkOutputDatum $ parseOutputDatum $ getTxOutHasScoringToken)

  where 
    -- Get all info about the transaction.
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    -- Get all inputs.
    allTxIn :: [PlutusV2.TxInInfo]
    allTxIn = PlutusV2.txInfoInputs info

    -- Get all outputs
    allTxOut :: [PlutusV2.TxOut]
    allTxOut = PlutusV2.txInfoOutputs info

     -- Check whether this transaction has the operator token in inputs.
    ownOperatorTokenInInput :: Bool
    ownOperatorTokenInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken mParams) >= 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

     -- Get the Scoring Token.
    scoringToken :: PlutusV2.Value
    scoringToken = PlutusV2.txInfoMint info

    -- Check the minted amount, it must be equal 1.
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue scoringToken of
      [(_, _, amount)] -> amount == 1
      _                -> False

    -- Check if the user's base score is good enough to mint Scoring Token.
    checkMinScoreToMintScoringToken :: Bool
    checkMinScoreToMintScoringToken =
      getBaseScore (pointsOfFactors rParams) (weights rParams) >= minScoreToMintScoringToken mParams

    {-
    This function will check whether the Scoring Token is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasScoringToken :: PlutusV2.TxOut
    getTxOutHasScoringToken =
      case find (\x -> Value.symbols (PlutusV2.txOutValue x) == Value.symbols scoringToken) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the Scoring Token in outputs"
        Just i  -> i

    -- Parse output datum to TokenInfo format.
    parseOutputDatum :: PlutusV2.TxOut -> Maybe TokenInfo
    parseOutputDatum txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od
                                        Nothing -> Nothing

    -- Check output datum.
    checkOutputDatum :: Maybe TokenInfo -> Bool
    checkOutputDatum tokenInfo = case tokenInfo of
      Just (TokenInfo ownerPKH ownerSH baseScore lendingScore lendingPackage latePayment) ->
        traceIfFalse "[Plutus Error]: ownerPKH must not be empty"
          (PlutusV2.getPubKeyHash ownerPKH /= "") &&

        traceIfFalse "[Plutus Error]: ownerSH must not be empty"
          (PlutusV2.getScriptHash ownerSH /= "") &&

        traceIfFalse "[Plutus Error]: base score must be correct based on points and weights"
          (baseScore == getBaseScore (pointsOfFactors rParams) (weights rParams)) &&

        traceIfFalse "[Plutus Error]: lendingScore must be 0 in initialize"
          (lendingScore == 0) &&

        traceIfFalse "[Plutus Error]: lendingPackage must be 0 in initialize"
          (lendingPackage == 0) &&

        traceIfFalse "[Plutus Error]: latePayment must be False in initialize"
          (latePayment == False)

      Nothing -> traceError "[Plutus Error]: output datum must not be empty"

policy :: MintParams -> Scripts.MintingPolicy
policy params = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
  where
    wrap params' = Scripts.mkUntypedMintingPolicy $ mkTokenPolicy params'

tokenSymbol :: MintParams -> PlutusV2.CurrencySymbol
tokenSymbol params = scriptCurrencySymbol $ policy params

script :: MintParams -> PlutusV2.Script
script params = PlutusV2.unMintingPolicyScript $ policy params

scriptSBS :: MintParams -> SBS.ShortByteString
scriptSBS params = SBS.toShort $ LBS.toStrict $ serialise $ script params

buildMintingContract :: MintParams -> PlutusScript PlutusScriptV2
buildMintingContract params = PlutusScriptSerialised $ scriptSBS params
