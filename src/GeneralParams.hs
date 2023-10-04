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
    TokenInfo(..)
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
+ minScoreToMintScoringToken: there is a threshold (a minimum score) to check if a user is able to receive the
Scoring Token or not.
+ managerContract: after minting, the Scoring Token will be sent to the ManageScoringToken contract only.
-}
data MintParams = MintParams
  {
    operatorToken              :: Value.AssetClass,
    minScoreToMintScoringToken :: Integer,
    managerContract            :: PlutusV2.ValidatorHash
  }
  deriving(Show)

PlutusTx.makeLift ''MintParams
PlutusTx.makeIsDataIndexed ''MintParams [('MintParams,0)]

{-
These parameters will be used in ManageScoringToken contract:
+ operatorToken: only operator is able to update new score for the Scoring Token.
+ minusPointsIfLatePayment:
-}
data ManageParams = ManageParams
  {
    operatorToken'           :: Value.AssetClass,
    minusPointsIfLatePayment :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''ManageParams
PlutusTx.makeIsDataIndexed ''ManageParams [('ManageParams,0)]

{-
These are information about the Scoring Token:
+ ownerPKH:
+ ownerSH:
+ baseScore:
+ lendingScore:
+ lendingPackage:
+ latePayment:
-}
data TokenInfo = TokenInfo 
  {
    ownerPKH       :: PlutusV2.PubKeyHash,
    ownerSH        :: PlutusV2.ScriptHash,
    baseScore      :: Integer,
    lendingScore   :: Integer,
    lendingPackage :: Integer,
    latePayment    :: Bool
  }
  deriving(Show)

PlutusTx.makeLift ''TokenInfo
PlutusTx.makeIsDataIndexed ''TokenInfo [('TokenInfo,0)]
