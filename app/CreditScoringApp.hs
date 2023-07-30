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
{-# LANGUAGE LambdaCase            #-}

import           Cardano.Api                hiding (TxId)
import qualified Plutus.V2.Ledger.Api       as PlutusV2
import qualified Data.ByteString.Char8      as C
import qualified Data.Aeson.Extras          as JSON
import qualified Data.Text                  as T
import qualified Plutus.Script.Utils.Value  as Value
import qualified PlutusTx.Builtins.Class    as BC
import           PlutusTx.Prelude           as P hiding (Semigroup (..), unless, (.))
import           Prelude                    (IO, (.))
import qualified Prelude                    as Haskell
import           System.Environment
import           CreditScoringContract
import           GeneralParams
import           Utility

main :: IO ()
main = do
  [tokenPolicy, tokenName, minScore] <- getArgs

  let operatorParams = OperatorParams {
    operatorToken = Value.AssetClass (toCurrencySymbol tokenPolicy, (Value.TokenName . BC.toBuiltin . C.pack) tokenName),
    minScoreToMintNFT = Haskell.read minScore :: Integer
  }

  let contract = "built-contracts/credit-scoring.json"
  
  result <- writeFileTextEnvelope contract Nothing $ mintScoringNFT operatorParams
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built credit scoring contract successfully at: " ++ contract
