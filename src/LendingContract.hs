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

data LendingParams = 
  PackageOne
  {
    fromPoint     :: Integer,
    toPoint       :: Integer,
    lendingAmount :: Integer
  } |
  PackageTwo 
  {
    fromPoint     :: Integer,
    toPoint       :: Integer,
    lendingAmount :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''LendingParams
PlutusTx.makeIsDataIndexed ''LendingParams [('PackageOne,0),('PackageTwo,1)]

data DatumParams = DatumParams 
  {
    score :: Integer,
    owner :: PlutusV2.PubKeyHash
  }
  deriving(Show)

PlutusTx.makeLift ''DatumParams
PlutusTx.makeIsDataIndexed ''DatumParams [('DatumParams,0)]

data RedeemerParams = LEND { packageNumber :: Integer } | PAYBACK
  deriving(Show)

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('LEND,0),('PAYBACK,1)]

{-# INLINABLE mkValidator #-}
mkValidator :: LendingParams -> DatumParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator lParams dParams rParams scriptContext = True

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
