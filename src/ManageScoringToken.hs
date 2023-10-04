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
{-# LANGUAGE NumericUnderscores    #-}

module ManageScoringToken
  (
    buildManagerContract,
    validator,
    RedeemerParams(..)
  )
where

import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import qualified Plutus.Script.Utils.Value            as Value
import qualified Plutus.V1.Ledger.Interval            as Interval
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),
                                                                   unless, (.))
import           Prelude                              (Show (..), (.))
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

-- This is the validator function for contract ManageScoringToken
{-# INLINABLE mkValidator #-}
mkValidator :: ManageParams -> TokenInfo -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator mParams tokenInfo rParams scriptContext =
    traceIfFalse "[Plutus Error]: you're not the operator to update the Scoring Token"
      ownOperatorTokenInInput &&

    traceIfFalse "[Plutus Error]: cannot find the Scoring Token in input"
      ((Value.isAdaOnlyValue theScoringTokenInInput) == False) &&

    traceIfFalse "[Plutus Error]: the Scoring Token in output must be sent to the ManageScoringToken contract only"
      (checkScoringTokenInOutput thisContractAddress) &&

    traceIfFalse "[Plutus Error]: output datum is not correct"
      (checkOutputDatum $ parseOutputDatumInTxOut $ getTxOutHasScoringToken)

  where
    -- Get all info about the transaction
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    -- Get the valid time range of this transaction
    range :: PlutusV2.POSIXTimeRange
    range = PlutusV2.txInfoValidRange info

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
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken' mParams) >= 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    -- This input will contain the Scoring Token
    mainInput :: PlutusV2.TxOut
    mainInput = case PlutusV2.findOwnInput scriptContext of
      Nothing -> traceError "[Plutus Error]: cannot find the input associated with the ManageScoringToken contract"
      Just i  -> PlutusV2.txInInfoResolved i

    -- Get the ManageScoringToken contract's address associated with the Scoring Token in input
    thisContractAddress :: PlutusV2.Address
    thisContractAddress = PlutusV2.txOutAddress mainInput

    -- Get the value of the Scoring Token
    theScoringTokenInInput :: PlutusV2.Value
    theScoringTokenInInput = PlutusV2.txOutValue mainInput

    {-
    This function is to check which address that the Scoring Token has been sent to in outputs.
    -}
    checkScoringTokenInOutput :: PlutusV2.Address -> Bool
    checkScoringTokenInOutput address =
      case find (
        \x -> (PlutusV2.txOutAddress x == address) &&
        (Value.symbols (PlutusV2.txOutValue x) == Value.symbols theScoringTokenInInput)
      ) allTxOut of
        Nothing -> False
        Just _  -> True

    {-
    This function will check whether the Scoring Token is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasScoringToken :: PlutusV2.TxOut
    getTxOutHasScoringToken =
      case find (\x -> Value.symbols (PlutusV2.txOutValue x) == Value.symbols theScoringTokenInInput) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the Scoring Token in output"
        Just i  -> i

    -- Parse output datum to the TokenInfo format
    parseOutputDatumInTxOut :: PlutusV2.TxOut -> Maybe TokenInfo
    parseOutputDatumInTxOut txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing

    -- Check output datum.
    checkOutputDatum :: Maybe TokenInfo -> Bool
    checkOutputDatum outputDatum = case outputDatum of
      Just (TokenInfo ownerPKH' ownerSH' newBaseScore lendingScore' lendingPackage' deadlinePayback') ->
        traceIfFalse "[Plutus Error]: ownerPKH must not been changed"
          (ownerPKH' == ownerPKH tokenInfo) &&

        traceIfFalse "[Plutus Error]: ownerSH must not been changed"
          (ownerSH' == ownerSH tokenInfo) &&

        traceIfFalse "[Plutus Error]: new base score must be correct based on new points and weights"
          (newBaseScore == getBaseScore (pointsOfFactors rParams) (weights rParams)) &&

        traceIfFalse "[Plutus Error]: lendingPackage must not been changed"
          (lendingPackage' == lendingPackage tokenInfo) &&

        traceIfFalse "[Plutus Error]: deadlinePayback must not been changed"
          (deadlinePayback' == deadlinePayback tokenInfo) &&

        case (Interval.before deadlinePayback' range) of
          False ->
            traceIfFalse "[Plutus Error]: lendingScore must not been changed"
              (lendingScore' == lendingScore tokenInfo)

          True ->
            traceIfFalse "[Plutus Error]: lendingScore must be decreased because of late payment"
              (lendingScore' == (lendingScore tokenInfo) - (minusPointsIfLatePayment mParams))

      Nothing -> traceError "[Plutus Error]: output datum must not be empty"

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = TokenInfo
  type RedeemerType ContractType = RedeemerParams

typedValidator :: ManageParams -> PlutusV2.TypedValidator ContractType
typedValidator = PlutusV2.mkTypedValidatorParam @ContractType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: ManageParams -> PSU.V2.Validator
validator = Scripts.validatorScript . typedValidator

script :: ManageParams -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptSBS :: ManageParams -> SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise . script

buildManagerContract :: ManageParams -> PlutusScript PlutusScriptV2
buildManagerContract = PlutusScriptSerialised . scriptSBS
