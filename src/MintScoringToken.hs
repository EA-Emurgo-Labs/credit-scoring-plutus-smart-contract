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
    saveMintCode,
    tokenSymbol
  )
where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                 PlutusScriptV2,
                                                 displayError,
                                                 writeFileTextEnvelope)
import           Codec.Serialise
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import qualified Ledger.Typed.Scripts            as Scripts
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified Plutus.V2.Ledger.Contexts       as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                as P hiding ((.))
import           Prelude                         (FilePath, IO, putStrLn, print, Show(..), (.))
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
      + Weight 0: weight of address balance
      + Weight 1: weight of staking reward
      + Weight 2: weight of payment frequency per month
      + Weight 3: weight of monthly payment
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

    traceIfFalse "[Plutus Error]: user's score is not good enough to receive the Scoring Token"
      checkMinScoreToMintScoringToken &&

    traceIfFalse "[Plutus Error]: minted amount must be one at a time"
      checkMintedAmount &&

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

    -- Check if the user's base score is good enough to receive the Scoring Token.
    checkMinScoreToMintScoringToken :: Bool
    checkMinScoreToMintScoringToken =
      getBaseScore (pointsOfFactors rParams) (weights rParams) >= minScore mParams

    {-
    This function will check whether the Scoring Token is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasScoringToken :: PlutusV2.TxOut
    getTxOutHasScoringToken =
      case find (\x -> Value.symbols (PlutusV2.txOutValue x) == Value.symbols scoringToken) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the Scoring Token in outputs"
        Just i  -> i

    -- Parse output datum to Scoring Token's format.
    parseOutputDatum :: PlutusV2.TxOut -> Maybe ScoringTokenInfo
    parseOutputDatum txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od
                                        Nothing -> Nothing

    -- Check output datum.
    checkOutputDatum :: Maybe ScoringTokenInfo -> Bool
    checkOutputDatum tokenInfo = case tokenInfo of
      Just (ScoringTokenInfo ownerPKH ownerSH baseScore lendingScore lendingAmount deadlinePayback) ->
        traceIfFalse "[Plutus Error]: ownerPKH must not be empty"
          (PlutusV2.getPubKeyHash ownerPKH /= "") &&

        traceIfFalse "[Plutus Error]: ownerSH must not be empty"
          (PlutusV2.getScriptHash ownerSH /= "") &&

        traceIfFalse "[Plutus Error]: base score must be correct based on points and weights"
          (baseScore == getBaseScore (pointsOfFactors rParams) (weights rParams)) &&

        traceIfFalse "[Plutus Error]: lendingScore must be 0 in initialize"
          (lendingScore == 0) &&

        traceIfFalse "[Plutus Error]: lendingAmount must be 0 in initialize"
          (lendingAmount == 0) &&

        traceIfFalse "[Plutus Error]: deadlinePayback must be 0 in initialize"
          (deadlinePayback == 0)

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

---------------------------------------------------------------------------------------------------------------
------------------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- This is another version for manager contract, it uses dynamic params to build the parameterized contract

{-# INLINABLE wrapValidator #-}
wrapValidator :: (RedeemerParams -> PlutusV2.ScriptContext -> Bool) -- ^
  -> (BuiltinData -> BuiltinData -> ())
wrapValidator f a ctx =
  check $
  f (PlutusTx.unsafeFromBuiltinData a)
    (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator oSymbol oName mScore = wrapValidator $ mkTokenPolicy appParams
  where
    oSymbol' :: PlutusV2.CurrencySymbol
    oSymbol' = PlutusTx.unsafeFromBuiltinData oSymbol

    oName' :: PlutusV2.TokenName
    oName' = PlutusTx.unsafeFromBuiltinData oName

    mScore' :: Integer
    mScore' = PlutusTx.unsafeFromBuiltinData mScore

    appParams :: MintParams
    appParams = MintParams {
      operatorToken = Value.AssetClass (oSymbol', oName'),
      minScore      = mScore'
    }

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [|| mkWrappedValidator ||])

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript =
  PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

-- Serialize compiled code
codeToScript :: PlutusTx.CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = serializableToScript . PlutusV2.fromCompiledCode

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath plutusScript =
  writeFileTextEnvelope filePath Nothing plutusScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized plutus script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> PlutusTx.CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

saveMintCode :: IO ()
saveMintCode =
  writeCodeToFile
    "./built-contracts/mint-score-parameterized.json"
    validatorCode
