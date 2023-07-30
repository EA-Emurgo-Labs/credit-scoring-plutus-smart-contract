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
    OperatorParams(..),
    LendingParams(..),
    NFTInfo(..)
  )
where

import qualified Plutus.Script.Utils.Value            as Value
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding (Semigroup (..), unless, (.))
import           Prelude                              (Show (..))

data OperatorParams = OperatorParams
  {
    operatorToken     :: Value.AssetClass,
    minScoreToMintNFT :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''OperatorParams
PlutusTx.makeIsDataIndexed ''OperatorParams [('OperatorParams,0)]

data LendingParams = LendingParams
  {
    operatorToken'      :: Value.AssetClass,

    {-
      Each lending package includes:
      + packageNumber   :: Integer,
      + fromPoint       :: Integer,
      + toPoint         :: Integer,
      + lendingAmount   :: Integer,
      + lendingInterest :: Integer
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
