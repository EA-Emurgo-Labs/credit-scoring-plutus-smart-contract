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
import qualified Plutus.V1.Ledger.Address             as Address
import qualified PlutusTx.Builtins                    as Builtins
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding (Semigroup (..),
                                                                   unless, (.))
import           Prelude                              (Show (..), (.))
import           GeneralParams
import           Utility

data RedeemerParams =
  OperatorRecalculateBaseScore
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
  | UserUpdateFlagsBorrow
  | UserUpdateFlagsPayback

  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [
  ('OperatorRecalculateBaseScore,0),
  ('UserUpdateFlagsBorrow,1),
  ('UserUpdateFlagsPayback,2)]

-- This is the validator function for contract ManageScoringToken
{-# INLINABLE mkValidator #-}
mkValidator :: ManageParams -> ScoringTokenInfo -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator mParams tokenInfo rParams scriptContext =
  traceIfFalse "[Plutus Error]: cannot find the Scoring Token in input"
    hasScoringTokenInInput &&

  traceIfFalse "[Plutus Error]: the Scoring Token in output must be sent to the Manager contract only"
    (Value.assetClassValueOf (PlutusV2.txOutValue $ getContinuingOutput) (scoringToken mParams) == 1) &&

  case rParams of
    OperatorRecalculateBaseScore pointsOfFactors weights ->
      traceIfFalse "[Plutus Error]: you're not the operator to update the Scoring Token"
        ownOperatorTokenInInput &&

      traceIfFalse "[Plutus Error]: output datum (recalculate base score) is not correct"
        (checkOutputDatumRecalculate pointsOfFactors weights outputScoringTokenInfo)

    UserUpdateFlagsBorrow ->
      traceIfFalse "[Plutus Error]: you're not the Scoring Token's owner"
        (PlutusV2.txSignedBy info (ownerPKH tokenInfo)) &&

      -- You have already borrowed a lending package and have not paid back yet.
      traceIfFalse "[Plutus Error]: you have already borrowed a package"
        (lendingAmount tokenInfo == 0) &&

      -- Check if the user's score is suitable with the lending package or not.
      traceIfFalse "[Plutus Error]: your score is not good enough to borrow this package"
        (
          (baseScore tokenInfo) + (lendingScore tokenInfo) >= (fromPoint getLendingPackageInfo)
        ) &&

      -- The value has been sent to user address must be correct.
      traceIfFalse "[Plutus Error]: value has been sent to user address must be correct"
        checkAmountSentToUser &&

      -- The lending fee (revenue) has been sent to the operator address must be correct.
      traceIfFalse "[Plutus Error]: lending fee has been sent to operator address must be correct"
        checkLendingFee &&

      traceIfFalse "[Plutus Error]: output datum (borrow) is not correct"
        (checkOutputDatumBorrow outputScoringTokenInfo)

    UserUpdateFlagsPayback ->
      traceIfFalse "[Plutus Error]: you're not the Scoring Token's owner"
        (PlutusV2.txSignedBy info (ownerPKH tokenInfo))
    
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
    hasScoringTokenInInput :: Bool
    hasScoringTokenInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (scoringToken mParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    getContinuingOutput :: PlutusV2.TxOut
    getContinuingOutput =
      case PlutusV2.getContinuingOutputs scriptContext of
        [i] -> i
        _   -> traceError "[Plutus Error]: cannot find the continuing output"

    -- Check whether this transaction has the operator token in inputs.
    ownOperatorTokenInInput :: Bool
    ownOperatorTokenInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken' mParams) >= 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    {-
    This function will check whether the Scoring Token is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasScoringToken :: PlutusV2.TxOut
    getTxOutHasScoringToken =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue x) (scoringToken mParams) == 1
      ) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the Scoring Token in output"
        Just i  -> i

    -- Parse output datum to the TokenInfo format
    parseScoringTokenInfo :: PlutusV2.TxOut -> Maybe ScoringTokenInfo
    parseScoringTokenInfo txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing
    
    outputScoringTokenInfo :: ScoringTokenInfo
    outputScoringTokenInfo = case parseScoringTokenInfo $ getTxOutHasScoringToken of
      Nothing -> traceError "[Plutus Error]: cannot find the Scoring Token's info in output"
      Just i  -> i

    -- Check output datum.
    checkOutputDatumRecalculate :: [Integer] -> [Integer] -> ScoringTokenInfo -> Bool
    checkOutputDatumRecalculate pointsOfFactors weights outputDatum =
      traceIfFalse "[Plutus Error]: ownerPKH must be unchanged"
        (ownerPKH outputDatum == ownerPKH tokenInfo) &&

      traceIfFalse "[Plutus Error]: ownerSH must be unchanged"
        (ownerSH outputDatum == ownerSH tokenInfo) &&

      traceIfFalse "[Plutus Error]: new base score must be correct based on new points and weights"
        (baseScore outputDatum == getBaseScore pointsOfFactors weights) &&

      traceIfFalse "[Plutus Error]: lendingAmount must be unchanged"
        (lendingAmount outputDatum == lendingAmount tokenInfo) &&

      traceIfFalse "[Plutus Error]: deadlinePayback must be unchanged"
        (deadlinePayback outputDatum == deadlinePayback tokenInfo) &&

      case (lendingAmount tokenInfo /= 0 && Interval.before (deadlinePayback tokenInfo) range) of
        False ->
          traceIfFalse "[Plutus Error]: lendingScore must be unchanged"
            (lendingScore outputDatum == lendingScore tokenInfo)

        True ->
          traceIfFalse "[Plutus Error]: lendingScore must be decreased because of late payment"
            (lendingScore outputDatum == (lendingScore tokenInfo) - (biasPoints mParams))

    getTxInHasLendingPackage :: PlutusV2.TxOut
    getTxInHasLendingPackage =
      case find (
        \x -> (PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Address.scriptHashAddress (lendingContract mParams))
      ) allTxIn of
        Nothing -> traceError "[Plutus Error]: cannot find the lending package in input"
        Just i  -> PlutusV2.txInInfoResolved i

    parseLendingPackageInfo :: PlutusV2.TxOut -> Maybe LendingPackageInfo
    parseLendingPackageInfo txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing

    getLendingPackageInfo :: LendingPackageInfo
    getLendingPackageInfo = case parseLendingPackageInfo $ getTxInHasLendingPackage of
      Nothing -> traceError "[Plutus Error]: cannot find the lending package's info in input"
      Just i  -> i


    -- Check if user receive the correct value when borrowing from Lending contract.
    checkAmountSentToUser :: Bool
    checkAmountSentToUser =
      case find (
        \x -> Value.valueOf x Value.adaSymbol Value.adaToken ==
                Builtins.divideInteger ((amount getLendingPackageInfo) * 1_000_000 * (100 - interest getLendingPackageInfo)) 100 
      ) (PlutusV2.pubKeyOutputsAt (ownerPKH tokenInfo) info) of
        Nothing -> False
        Just _  -> True

    -- Check the lending fee.
    checkLendingFee :: Bool
    checkLendingFee =
      case find (
        \x -> Value.valueOf x Value.adaSymbol Value.adaToken ==
                Builtins.divideInteger ((amount getLendingPackageInfo) * 1_000_000 * (interest getLendingPackageInfo)) 100  
      ) (PlutusV2.pubKeyOutputsAt (operatorAddr mParams) info) of
        Nothing -> False
        Just _  -> True

    -- Check output datum.
    checkOutputDatumBorrow :: ScoringTokenInfo -> Bool
    checkOutputDatumBorrow outputDatum =
      traceIfFalse "[Plutus Error]: ownerPKH must be unchanged"
        (ownerPKH outputDatum == ownerPKH tokenInfo) &&

      traceIfFalse "[Plutus Error]: ownerSH must be unchanged"
        (ownerSH outputDatum == ownerSH tokenInfo) &&

      traceIfFalse "[Plutus Error]: baseScore must be unchanged"
        (baseScore outputDatum == baseScore tokenInfo) &&

      traceIfFalse "[Plutus Error]: lendingScore must be unchanged"
        (lendingScore outputDatum == lendingScore tokenInfo) &&

      traceIfFalse "[Plutus Error]: lendingAmount must be updated"
        (lendingAmount outputDatum == amount getLendingPackageInfo) &&

      traceIfFalse "[Plutus Error]: deadlinePayback must be updated"
        (deadlinePayback outputDatum == deadline getLendingPackageInfo)

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ScoringTokenInfo
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
