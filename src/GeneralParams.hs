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
These parameters will be used in Credit Scoring contract:
+ operatorToken: only operator is able to mint a new Scoring NFT (attach the score in datum) for user.
+ minScoreToMintNFT: there is a threshold (a minimum score) to check if a user is able to receive the
Scoring NFT or not.
-}
data MintParams = MintParams
  {
    operatorToken              :: Value.AssetClass,
    minScoreToMintScoringToken :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''MintParams
PlutusTx.makeIsDataIndexed ''MintParams [('MintParams,0)]

data ManageParams = ManageParams
  {
    operatorToken'           :: Value.AssetClass,
    minusPointsIfLatePayment :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''ManageParams
PlutusTx.makeIsDataIndexed ''ManageParams [('ManageParams,0)]

{-
These are information about the Scoring NFT:
+ score: it will be calculated based on both onchain and offchain data for each user.
+ owner: the Scoring NFT's owner.
+ lendingPackage: it will be 0 in initialize, and will be updated with the packageNumber that user borrowed
from Lending contract.
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
