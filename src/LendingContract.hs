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

data LendingParams = LendingParams
  {
    {-
      Each lending package includes:
      + packageNumber :: Integer,
      + fromPoint     :: Integer,
      + toPoint       :: Integer,
      + lendingAmount :: Integer
    -}
    lendingPackagesInfo :: [(Integer, Integer, Integer, Integer)]
  }
  deriving(Show)

PlutusTx.makeLift ''LendingParams
PlutusTx.makeIsDataIndexed ''LendingParams [('LendingParams,0)]

data NFTInfo = NFTInfo 
  {
    score          :: Integer,
    owner          :: PlutusV2.PubKeyHash,
    lendingPackage :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''NFTInfo
PlutusTx.makeIsDataIndexed ''NFTInfo [('NFTInfo,0)]

data DatumParams = DatumParams 
  {
    packageNumber :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''DatumParams
PlutusTx.makeIsDataIndexed ''DatumParams [('DatumParams,0)]

data RedeemerParams = LEND | PAYBACK | CLAIM
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('LEND,0),('PAYBACK,1),('CLAIM,2)]

{-# INLINABLE mkValidator #-}
mkValidator :: LendingParams -> DatumParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator lParams dParams rParams scriptContext =   
    case rParams of
      LEND ->
        True
        -- You must be the Scoring NFT's owner to do this action

        -- The Scoring NFT must be in inputs (belongs to user address)

        -- Check if the user's score is suitable with the package number or not

        -- The value has been paid to user must be correct based on the package number

        -- The Scoring NFT must be sent to Lending Contract after that

        {- 
          Check output datum for NFT must be valid 
        -}

        -- score must be unchanged

        -- owner must be unchanged

        -- lendingPackage must be updated with the packageNumber

      PAYBACK -> 
        True
        -- You must be the Scoring NFT's owner to do this action

        -- The Scoring NFT must be in inputs (belongs to Lending Contract)

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
        True
        -- You must be the operator to do this action

  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

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
