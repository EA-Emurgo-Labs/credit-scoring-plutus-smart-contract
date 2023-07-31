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

module CreditScoringContract
  (
    mintScoringNFT
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
import           GeneralParams

{-
These parameters will include all onchain and offchain data to calculate the score for user.
-}
data RedeemerParams = RedeemerParams
  {
    {-
      For example:
      + Factor 0: age (value: 30 (years old), OR 35 (years old), ...)
      + Factor 1: salary (value: 1000$, OR 1500$, OR 2000$, ...)
      ...
    -} 
    factors :: [Integer],

    {-
      For example:
      + Weight 0: weight of age (value: 10, OR 20, ...)
      + Weight 1: weight of salary (value: 5, OR 7, ...)
      ...
    -}     
    weights :: [Integer]
  }
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('RedeemerParams,0)]

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: OperatorParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkNFTPolicy oParams rParams scriptContext =
    traceIfFalse "[Plutus Error]: you're not the operator to mint the Scoring NFT" ownOperatorTokenInInput &&
    traceIfFalse "[Plutus Error]: minted amount must be one" checkMintedAmount &&
    traceIfFalse "[Plutus Error]: your score is not enough to receive the Scoring NFT" checkMinScoreToMintNFT &&
    traceIfFalse "[Plutus Error]: output datum is not correct" (checkOutputDatum $ parseOutputDatum $ getTxOutHasNFT)
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
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken oParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

     -- Get the Scoring NFT.
    scoringNFT :: PlutusV2.Value
    scoringNFT = PlutusV2.txInfoMint info

    -- Check the minted amount, it must be equal 1.
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue scoringNFT of
      [(_, _, amount)] -> amount == 1
      _                -> False

    -- Check if the user's total score is good enough to mint Scoring NFT.
    checkMinScoreToMintNFT :: Bool
    checkMinScoreToMintNFT =
      getTotalScore (factors rParams) (weights rParams) >= minScoreToMintNFT oParams

    -- Calculate the user's total score based on factors and weights.
    getTotalScore :: [Integer] -> [Integer] -> Integer
    getTotalScore [] []         = 0
    getTotalScore _  []         = 0
    getTotalScore [] _          = 0
    getTotalScore [x] [y]       = x * y
    getTotalScore (x:xs) (y:ys) = x * y + getTotalScore xs ys

    {-
    This function will check whether the Scoring NFT is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasNFT :: PlutusV2.TxOut
    getTxOutHasNFT =
      case find (\x -> Value.symbols (PlutusV2.txOutValue x) == Value.symbols scoringNFT) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the Scoring NFT in outputs"
        Just i  -> i

    -- Parse output datum to NFTInfo format.
    parseOutputDatum :: PlutusV2.TxOut -> Maybe NFTInfo
    parseOutputDatum txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od
                                        Nothing -> Nothing

    -- Check output datum.
    checkOutputDatum :: Maybe NFTInfo -> Bool
    checkOutputDatum nftInfo = case nftInfo of
      Just (NFTInfo score owner lendingPackage) ->
        traceIfFalse "[Plutus Error]: score must be valid" (score == getTotalScore (factors rParams) (weights rParams)) &&
        traceIfFalse "[Plutus Error]: owner must not be empty" (PlutusV2.getPubKeyHash owner /= "") &&
        traceIfFalse "[Plutus Error]: lendingPackage must be 0 in intialize" (lendingPackage == 0)

      Nothing -> traceError "[Plutus Error]: output datum must not be empty"

policy :: OperatorParams -> Scripts.MintingPolicy
policy params = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
  where
    wrap params' = Scripts.mkUntypedMintingPolicy $ mkNFTPolicy params'

script :: OperatorParams -> PlutusV2.Script
script params = PlutusV2.unMintingPolicyScript $ policy params

scriptSBS :: OperatorParams -> SBS.ShortByteString
scriptSBS params = SBS.toShort $ LBS.toStrict $ serialise $ script params

mintScoringNFT :: OperatorParams -> PlutusScript PlutusScriptV2
mintScoringNFT params = PlutusScriptSerialised $ scriptSBS params
