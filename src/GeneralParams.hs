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
import           PlutusTx.Prelude                     as P hiding ((.))
import           Prelude                              (Show (..))

{-
These parameters will be used in Credit Scoring contract:
+ operatorToken: only operator is able to mint a new Scoring NFT (attach the score in datum) for user.
+ minScoreToMintNFT: there is a threshold (a minimum score) to check if a user is able to receive the
Scoring NFT or not.
-}
data OperatorParams = OperatorParams
  {
    operatorToken     :: Value.AssetClass,
    minScoreToMintNFT :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''OperatorParams
PlutusTx.makeIsDataIndexed ''OperatorParams [('OperatorParams,0)]

{-
These parameters will be used in Lending contract:
+ operatorToken': only operator is able to claim back all the money from Lending contract.
+ scoringNFT: if user want to borrow money from Lending contract, they have to own a Scoring NFT.
+ lendingPackagesInfo: all the information about lending packages.
-}
data LendingParams = LendingParams
  {
    operatorToken'      :: Value.AssetClass,
    scoringNFT          :: Value.AssetClass,

    {-
      Each lending package includes:
      + packageNumber     :: Integer, -- start from index 1
      + fromPoint         :: Integer, -- min point for this package
      + toPoint           :: Integer, -- max point for this package
      + lendingAmount     :: Integer, -- amount that user can borrow
      --------------------------------------------------------------------
      + lendingInterest   :: Integer, -- will implement this feature later
      + deadlineToPayback :: Integer  -- will implement this feature later
    -}
    lendingPackagesInfo :: [(Integer, Integer, Integer, Integer)]
  }
  deriving(Show)

PlutusTx.makeLift ''LendingParams
PlutusTx.makeIsDataIndexed ''LendingParams [('LendingParams,0)]

{-
These are information about the Scoring NFT:
+ score: it will be calculated based on both onchain and offchain data for each user.
+ owner: the Scoring NFT's owner.
+ lendingPackage: it will be 0 in initialize, and will be updated with the packageNumber that user borrowed
from Lending contract.
-}
data NFTInfo = NFTInfo 
  {
    score          :: Integer,
    owner          :: PlutusV2.PubKeyHash,
    lendingPackage :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''NFTInfo
PlutusTx.makeIsDataIndexed ''NFTInfo [('NFTInfo,0)]
