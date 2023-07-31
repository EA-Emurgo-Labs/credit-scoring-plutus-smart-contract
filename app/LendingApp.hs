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
import           LendingContract
import           GeneralParams
import           Utility

main :: IO ()
main = do
  -- Get info of operator token and Scoring NFT.
  [tokenPolicy, tokenName, scoringNFTPolicy, scoringNFTName] <- getArgs

  -- Get info of all lending packages.
  lendingArgs <- getArgs

  -- Parse arguments for lending packages.
  let lendingPackagesInfo' = parseArgs lendingArgs

  -- Construct params for Lending contract
  let lendingParams = LendingParams {
    operatorToken' = Value.AssetClass (toCurrencySymbol tokenPolicy, (Value.TokenName . BC.toBuiltin . C.pack) tokenName),
    scoringNFT = Value.AssetClass (toCurrencySymbol scoringNFTPolicy, (Value.TokenName . BC.toBuiltin . C.pack) scoringNFTName),
    lendingPackagesInfo = lendingPackagesInfo'
  }

  let contract = "built-contracts/lending.json"
  
  -- Built the plutus script for Lending contract.
  result <- writeFileTextEnvelope contract Nothing $ buildLendingContract lendingParams
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built lending contract successfully at: " ++ contract

parseArgs :: [Haskell.String] -> [(Integer, Integer, Integer, Integer)]
parseArgs [] = []
parseArgs (packageNumber:fromPoint:toPoint:lendingAmount:xs) =
  [( Haskell.read packageNumber :: Integer,
     Haskell.read fromPoint     :: Integer,
     Haskell.read toPoint       :: Integer,
     Haskell.read lendingAmount :: Integer )] ++ parseArgs xs
