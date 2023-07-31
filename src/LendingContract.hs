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

module LendingContract
  (
    buildLendingContract,
    LendingParams(..)
  )
where

import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2,
                                                       displayError,
                                                       writeFileTextEnvelope)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import qualified Plutus.Script.Utils.Value            as Value
import qualified Plutus.V1.Ledger.Address             as Address
import qualified Plutus.V1.Ledger.Interval            as Interval
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (FilePath, IO, Show (..),
                                                       print, putStrLn, (.))
import           GeneralParams
import           Utility

data DatumParams = DatumParams 
  {
    packageNumber :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''DatumParams
PlutusTx.makeIsDataIndexed ''DatumParams [('DatumParams,0)]

data RedeemerParams = BORROW | PAYBACK | CLAIM
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('BORROW,0),('PAYBACK,1),('CLAIM,2)]

{-# INLINABLE mkValidator #-}
mkValidator :: LendingParams -> DatumParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator lParams dParams rParams scriptContext =   
    case rParams of
      BORROW ->
        -- The Scoring NFT must be in inputs (belongs to user address)
        traceIfFalse "[PlutusError]: cannot find the Scoring NFT in inputs"
          ownScoringNFTInInput &&

        -- You must be the Scoring NFT's owner to do this action
        traceIfFalse "[Plutus Error]: you're not the Scoring NFT's owner"
          (PlutusV2.txSignedBy info (owner getNFTInfo)) &&

        -- Check if the user's score is suitable with the package number or not
        traceIfFalse "[Plutus Error]: your score is not good enough to borrow this package"
          (checkScore $ packageNumber dParams)

        -- The value has been paid to user must be correct based on the package number

        -- The Scoring NFT must be sent to Lending Contract after that

        {- 
          Check output datum for NFT must be valid 
        -}

        -- score must be unchanged

        -- owner must be unchanged

        -- lendingPackage must be updated with the packageNumber

      PAYBACK ->
        -- The Scoring NFT must be in inputs (belongs to Lending Contract)
        traceIfFalse "[PlutusError]: cannot find the Scoring NFT in inputs"
          ownScoringNFTInInput &&

        -- You must be the Scoring NFT's owner to do this action
        traceIfFalse "[Plutus Error]: you're not the Scoring NFT's owner"
          (PlutusV2.txSignedBy info (owner getNFTInfo))


        -- Check if money has been sent back to Lending Contract

        {- 
          Check output datum for Lending Contract must be valid 
        -}

        -- packageNumber must be correct

        -- The Scoring NFT must be sent back to user address

        {- 
          Check output datum for NFT must be valid 
        -}

        -- score must be unchanged

        -- owner must be unchanged

        -- lendingPackage must be reset to 0

      CLAIM ->
        traceIfFalse "[Plutus Error]: you're not the operator" ownOperatorTokenInInput

  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    -- Get all inputs
    allTxIn :: [PlutusV2.TxInInfo]
    allTxIn = PlutusV2.txInfoInputs info

    ownScoringNFTInInput :: Bool
    ownScoringNFTInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (scoringNFT lParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    getTxInHasScoringNFT :: PlutusV2.TxOut
    getTxInHasScoringNFT =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (scoringNFT lParams) == 1
      ) allTxIn of
        Nothing -> traceError "[PlutusError]: cannot find the Scoring NFT in inputs"
        Just i  -> PlutusV2.txInInfoResolved i 

    parseNFTInfo :: PlutusV2.TxOut -> Maybe NFTInfo
    parseNFTInfo txout = case PlutusV2.txOutDatum txout of
      PlutusV2.NoOutputDatum       -> Nothing
      PlutusV2.OutputDatum od      -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum od)
      PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
                                        Just od -> (PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od)
                                        Nothing -> Nothing

    getNFTInfo :: NFTInfo
    getNFTInfo = case parseNFTInfo $ getTxInHasScoringNFT of
      Nothing -> traceError "[PlutusError]: cannot find NFT info"
      Just i  -> i

    checkScore :: Integer -> Bool
    checkScore packageNumber' =
      case find (
        \x -> (first x == packageNumber' && second x <= score getNFTInfo)
      ) (lendingPackagesInfo lParams) of
        Nothing -> False
        Just _  -> True

    -- Check whether this transaction has the operator token in inputs
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
