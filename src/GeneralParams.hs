{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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

module GeneralParams
  (
    MintParams(..),
    ManageParams(..),
    ScoringTokenInfo(..),
    LendingPackageInfo(..)
  )
where

import qualified Plutus.Script.Utils.Value as Value
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding ((.))
import           Prelude                   (Show (..))

{-
These parameters will be used in MintScoringToken contract:
+ operatorToken: only operator is able to mint a new Scoring Token.
+ minScore: there is a threshold (a minimum score) to check if a user is able to receive the
Scoring Token or not.
-}
data MintParams = MintParams
  {
    operatorToken :: Value.AssetClass,
    minScore      :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''MintParams
PlutusTx.makeIsDataIndexed ''MintParams [('MintParams,0)]

{-
These parameters will be used in ManageScoringToken contract:
+ operatorToken: only operator is able to update new score for the Scoring Token.
+ minusPointsIfLatePayment: the lending score will be decreased if user has late payment in lending.
-}
data ManageParams = ManageParams
  {
    operatorToken'  :: Value.AssetClass,
    operatorAddr    :: PlutusV2.PubKeyHash,
    scoringToken    :: Value.AssetClass,
    lendingContract :: PlutusV2.ValidatorHash,
    biasPoints      :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''ManageParams
PlutusTx.makeIsDataIndexed ''ManageParams [('ManageParams,0)]

{-
These are information about the Scoring Token:
+ ownerPKH: owner's pubkey hash (address = pubkey hash + script hash).
+ ownerSH: owner's script hash (address = pubkey hash + script hash).
+ baseScore: this score will be re-calculated at the beginning of each month.
+ lendingScore: this score will be updated based on lending history.
+ lendingAmount: it is used to mark the package that user is borrowing from Lending contract.
+ deadlinePayback: it is used to check whether user has a late payment or not.
-}
data ScoringTokenInfo = ScoringTokenInfo 
  {
    ownerPKH        :: PlutusV2.PubKeyHash,
    ownerSH         :: PlutusV2.ScriptHash,
    baseScore       :: Integer,
    lendingScore    :: Integer,
    lendingAmount   :: Integer,
    deadlinePayback :: PlutusV2.POSIXTime
  }
  deriving(Show)

PlutusTx.makeLift ''ScoringTokenInfo
PlutusTx.makeIsDataIndexed ''ScoringTokenInfo [('ScoringTokenInfo,0)]

data LendingPackageInfo = LendingPackageInfo 
  {
    fromPoint        :: Integer,
    toPoint          :: Integer,
    amount           :: Integer,
    interest         :: Integer,
    deadline         :: PlutusV2.POSIXTime
  }
  deriving(Show)

PlutusTx.makeLift ''LendingPackageInfo
PlutusTx.makeIsDataIndexed ''LendingPackageInfo [('LendingPackageInfo,0)]
