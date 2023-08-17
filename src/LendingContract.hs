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

module LendingContract
  (
    buildLendingContract,
    validator,
    DatumParams(..),
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
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding ((.))
import           Prelude                              (Show (..), (.))
import           GeneralParams
import           Utility

{-
At the beginning, the operator will send money to Lending contract to create lending packages.
Each lending package has the packageNumber as a datum, the packageNumber will represent for all
information of the lending package, it means if we know the packageNumber, we can know all information
of this package. 
-}
data DatumParams = DatumParams 
  {
    packageNumber :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''DatumParams
PlutusTx.makeIsDataIndexed ''DatumParams [('DatumParams,0)]

{-
There are 3 cases to interact with the Lending contract:

+ BORROW: user want to borrow money from Lending contract, so they will make a transaction with attaching
the Scoring NFT to unlock money from Lending contract, then the Scoring NFT must be sent to Lending contract,
after that if they want to borrow again, they have to send back the money first.

+ PAYBACK: user pay back the money to Lending contract, and then get back their Scoring NFT.

+ CLAIM: only the operator is able to claim back all the money from Lending contract that they have sent to.
-}
data RedeemerParams = BORROW | PAYBACK | CLAIM
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('BORROW,0),('PAYBACK,1),('CLAIM,2)]

{-# INLINABLE mkValidator #-}
mkValidator :: LendingParams -> DatumParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator lParams dParams rParams scriptContext =   
    case rParams of
      BORROW ->
        -- The Scoring NFT must be in inputs (belongs to user address).
        traceIfFalse "[PlutusError]: cannot find the Scoring NFT in inputs"
          ownScoringNFTInInput &&

        -- -- You must be the Scoring NFT's owner to do this action.
        -- traceIfFalse "[Plutus Error]: you're not the Scoring NFT's owner"
        --   (PlutusV2.txSignedBy info (owner getNFTInfoInInput))

        -- -- Check if the user's score is suitable with the package number or not.
        -- traceIfFalse "[Plutus Error]: your score is not good enough to borrow this package"
        --   (score getNFTInfoInInput >= (second . getPackageInfo $ packageNumber dParams))

        -- -- The value has been sent to user address must be correct based on the package number.
        -- traceIfFalse "[Plutus Error]: value has been sent to user address must be correct"
        --   (checkReceivedAmount (owner getNFTInfoInInput) (packageNumber dParams))

        -- The Scoring NFT must be sent to Lending contract after that.
        traceIfFalse "[Plutus Error]: the Scoring NFT must be sent to Lending contract"
          (Value.assetClassValueOf (PlutusV2.txOutValue getContinuingOutput) (scoringNFT lParams) == 1)

        -- {- 
        --   Output datum of Scoring NFT must be valid:
        --     + score must be unchanged.
        --     + owner must be unchanged.
        --     + lendingPackage must be updated with the packageNumber.
        -- -}
        -- traceIfFalse "[Plutus Error]: output datum of Scoring NFT (borrow) is not correct"
        --   (checkOutputDatumNFTBorrow getNFTInfoInInput getNFTInfoInOutput)

      PAYBACK ->
        -- The Scoring NFT must be in inputs (belongs to Lending contract).
        traceIfFalse "[PlutusError]: cannot find the Scoring NFT in inputs"
          ownScoringNFTInInput &&

        -- You must be the Scoring NFT's owner to do this action.
        traceIfFalse "[Plutus Error]: you're not the Scoring NFT's owner"
          (PlutusV2.txSignedBy info (owner getNFTInfoInInput)) &&

        -- Check if money must be sent back to Lending contract with correct value.
        traceIfFalse "[Plutus Error]: money must be sent back to Lending contract with correct value"
          (
            Value.valueOf (PlutusV2.txOutValue getContinuingOutput) Value.adaSymbol Value.adaToken == 
              (fourth . getPackageInfo $ lendingPackage getNFTInfoInInput) * 1_000_000
          ) &&

        {- 
          Output datum of Lending contract must be valid to restore the lending package:
            + packageNumber must be correct based on the money that user pay back.
        -}
        traceIfFalse "[Plutus Error]: output datum of lending package is not correct"
          (checkOutputDatumParams (parseDatumParams getContinuingOutput) getNFTInfoInInput) &&

        -- The Scoring NFT must be sent back to user address.
        traceIfFalse "[Plutus Error]: the Scoring NFT must be sent back to user address"
          (
            Value.assetClassValueOf (PlutusV2.valuePaidTo info (owner getNFTInfoInInput)) (scoringNFT lParams) == 1
          ) &&

        {- 
          Output datum of Scoring NFT must be valid:
            + score must be unchanged.
            + owner must be unchanged.
            + lendingPackage must be reset to 0.
        -}
        traceIfFalse "[Plutus Error]: output datum of Scoring NFT (payback) is not correct"
          (checkOutputDatumNFTPayback getNFTInfoInInput getNFTInfoInOutput)

      CLAIM ->
        -- Only operator is able to claim back money from Lending contract.
        traceIfFalse "[Plutus Error]: you're not the operator" ownOperatorTokenInInput

  where
    -- Get all info about the transaction.
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    -- Get all inputs.
    allTxIn :: [PlutusV2.TxInInfo]
    allTxIn = PlutusV2.txInfoInputs info

    -- Get all outputs.
    allTxOut :: [PlutusV2.TxOut]
    allTxOut = PlutusV2.txInfoOutputs info

    -- Check if the Scoring NFT is in inputs.
    ownScoringNFTInInput :: Bool
    ownScoringNFTInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (scoringNFT lParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    -- Get the inputs that contain the Scoring NFT.
    getTxInHasScoringNFT :: PlutusV2.TxOut
    getTxInHasScoringNFT =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (scoringNFT lParams) == 1
      ) allTxIn of
        Nothing -> traceError "[PlutusError]: cannot find the Scoring NFT in inputs"
        Just i  -> PlutusV2.txInInfoResolved i 

    -- Get the outputs that contain the Scoring NFT.
    getTxOutHasScoringNFT :: PlutusV2.TxOut
    getTxOutHasScoringNFT =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue x) (scoringNFT lParams) == 1
      ) allTxOut of
        Nothing -> traceError "[PlutusError]: cannot find the Scoring NFT in outputs"
        Just i  -> i 

    -- Parse NFT's information.
    parseNFTInfo :: PlutusV2.TxOut -> Maybe NFTInfo
    parseNFTInfo txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing

    -- Parse the datum attached with lending package.
    parseDatumParams :: PlutusV2.TxOut -> Maybe DatumParams
    parseDatumParams txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing

    -- Get NFT's info in input.
    getNFTInfoInInput :: NFTInfo
    getNFTInfoInInput = case parseNFTInfo $ getTxInHasScoringNFT of
      Nothing -> traceError "[PlutusError]: cannot find NFT info attached with Scoring NFT in inputs"
      Just i  -> i

    -- Get NFT's info in output.
    getNFTInfoInOutput :: NFTInfo
    getNFTInfoInOutput = case parseNFTInfo $ getTxOutHasScoringNFT of
      Nothing -> traceError "[PlutusError]: cannot find NFT info attached with Scoring NFT in outputs"
      Just i  -> i

    -- Get full information about the lending package.
    getPackageInfo :: Integer -> (Integer, Integer, Integer, Integer)
    getPackageInfo packageNumber =
      case find (
        \x -> (first x == packageNumber)
      ) (lendingPackagesInfo lParams) of
        Nothing -> traceError "[Plutus Error]: cannot find the lending package info"
        Just p  -> p

    -- Check if user receive the correct value when borrowing from Lending contract
    checkReceivedAmount :: PlutusV2.PubKeyHash -> Integer -> Bool
    checkReceivedAmount owner' packageNumber' =
      case find (
        \x -> Value.valueOf x Value.adaSymbol Value.adaToken == (fourth . getPackageInfo $ packageNumber') * 1_000_000
      ) (PlutusV2.pubKeyOutputsAt owner' info) of
        Nothing -> False
        Just _  -> True

    -- Get the continuing output has been sent to Lending contract address.
    getContinuingOutput :: PlutusV2.TxOut
    getContinuingOutput =
      case PlutusV2.getContinuingOutputs scriptContext of
        [i] -> i
        _   -> traceError "[Plutus Error]: cannot find the continuing output"

    -- Check output datum of Scoring NFT in case of borrow.
    checkOutputDatumNFTBorrow :: NFTInfo -> NFTInfo -> Bool
    checkOutputDatumNFTBorrow nftInfoIn nftInfoOut =
      traceIfFalse "[Plutus Error]: score must be unchanged"
        (score nftInfoOut == score nftInfoIn) &&

      traceIfFalse "[Plutus Error]: owner must be unchanged"
        (owner nftInfoOut == owner nftInfoIn) &&

      traceIfFalse "[Plutus Error]: lendingPackage must be updated with the packageNumber"
        (lendingPackage nftInfoOut == packageNumber dParams)

    -- Check output datum of Scoring NFT in case of pay back.
    checkOutputDatumNFTPayback :: NFTInfo -> NFTInfo -> Bool
    checkOutputDatumNFTPayback nftInfoIn nftInfoOut =
      traceIfFalse "[Plutus Error]: score must be unchanged"
        (score nftInfoOut == score nftInfoIn) &&

      traceIfFalse "[Plutus Error]: owner must be unchanged"
        (owner nftInfoOut == owner nftInfoIn) &&

      traceIfFalse "[Plutus Error]: lendingPackage must be reset to 0"
        (lendingPackage nftInfoOut == 0)

    -- Check output datum of lending package.
    checkOutputDatumParams :: Maybe DatumParams -> NFTInfo -> Bool
    checkOutputDatumParams dParams' nftInfo = case dParams' of
      Just (DatumParams packageNumber) ->
        traceIfFalse "[Plutus Error]: packageNumber is not correct"
          (packageNumber == lendingPackage nftInfo)
      Nothing -> traceError "[Plutus Error]: cannot find output datum of lending package"

    -- Check whether this transaction has the operator token in inputs or not.
    ownOperatorTokenInInput :: Bool
    ownOperatorTokenInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken' lParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = DatumParams
  type RedeemerType ContractType = RedeemerParams

typedValidator :: LendingParams -> PlutusV2.TypedValidator ContractType
typedValidator = PlutusV2.mkTypedValidatorParam @ContractType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: LendingParams -> PSU.V2.Validator
validator = Scripts.validatorScript . typedValidator

script :: LendingParams -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptSBS :: LendingParams -> SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise . script

buildLendingContract :: LendingParams -> PlutusScript PlutusScriptV2
buildLendingContract = PlutusScriptSerialised . scriptSBS
