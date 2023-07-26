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

module CreditScoringContract
  (
    mintScoringNFT,
    OperatorParams(..)
  )
where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import qualified Ledger.Typed.Scripts            as Scripts
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                as P hiding (Semigroup (..),
                                                              unless, (.))
import           Prelude                         (Show(..))

data OperatorParams = OperatorParams
  {
    operatorToken     :: Value.AssetClass,
    minScoreToMintNFT :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''OperatorParams
PlutusTx.makeIsDataIndexed ''OperatorParams [('OperatorParams,0)]

data RedeemerParams = RedeemerParams
  {
    {-
      For example:
      + Factor 0: age (value: 30 (years old), OR 35 (years old), ...)
      + Factor 1: salary (value: 1000$, OR 1500$, OR 2000$, ...)
      ...
    -} 
    factors :: [Integer],

    {-
      For example:
      + Weight 0: weight of age (value: 10, OR 20, ...)
      + Weight 1: weight of salary (value: 5, OR 7, ...)
      ...
    -}     
    weights :: [Integer]
  }

PlutusTx.makeLift ''RedeemerParams
PlutusTx.makeIsDataIndexed ''RedeemerParams [('RedeemerParams,0)]

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: OperatorParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkNFTPolicy oParams rParams scriptContext =
    traceIfFalse "[Plutus Error]: you're not the operator to mint the Lending NFT" ownOperatorTokenInInput &&
    traceIfFalse "[Plutus Error]: minted amount must be one" checkMintedAmount &&
    traceIfFalse "[Plutus Error]: your score is not enough to receive the Scoring NFT" checkMinScoreToMintNFT
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    allTxIn :: [PlutusV2.TxInInfo]
    allTxIn = PlutusV2.txInfoInputs info

    ownOperatorTokenInInput :: Bool
    ownOperatorTokenInInput =
      case find (
        \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken oParams) == 1
      ) allTxIn of
        Nothing -> False
        Just _  -> True

    mintedNFT :: PlutusV2.Value
    mintedNFT = PlutusV2.txInfoMint info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue mintedNFT of
      [(_, _, amount)] -> amount == 1
      _                -> False

    checkMinScoreToMintNFT :: Bool
    checkMinScoreToMintNFT =
      getTotalScore (factors rParams) (weights rParams) >= minScoreToMintNFT oParams

    getTotalScore :: [Integer] -> [Integer] -> Integer
    getTotalScore [] []         = 0
    getTotalScore _  []         = 0
    getTotalScore [] _          = 0
    getTotalScore [x] [y]       = x * y
    getTotalScore (x:xs) (y:ys) = x * y + getTotalScore xs ys

policy :: OperatorParams -> Scripts.MintingPolicy
policy params = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
  where
    wrap params' = Scripts.mkUntypedMintingPolicy $ mkNFTPolicy params'

script :: OperatorParams -> PlutusV2.Script
script params = PlutusV2.unMintingPolicyScript $ policy params

scriptSBS :: OperatorParams -> SBS.ShortByteString
scriptSBS params = SBS.toShort $ LBS.toStrict $ serialise $ script params

mintScoringNFT :: OperatorParams -> PlutusScript PlutusScriptV2
mintScoringNFT params = PlutusScriptSerialised $ scriptSBS params
