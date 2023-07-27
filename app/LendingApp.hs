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

main :: IO ()
main = do
  [ fromPointPackage0,
    toPointPackage0,
    lendingAmountPackage0,
    fromPointPackage1,
    toPointPackage1,
    lendingAmountPackage1 ] <- getArgs

  let lendingParams = LendingParams {
    lendingPackages = [
      (
        Haskell.read fromPointPackage0     :: Integer,
        Haskell.read toPointPackage0       :: Integer,
        Haskell.read lendingAmountPackage0 :: Integer
      ),
      (
        Haskell.read fromPointPackage1     :: Integer,
        Haskell.read toPointPackage1       :: Integer,
        Haskell.read lendingAmountPackage1 :: Integer
      )      
    ]
  }

  let contract = "built-contracts/lending.json"
  
  result <- writeFileTextEnvelope contract Nothing $ buildLendingContract lendingParams
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built lending contract successfully at: " ++ contract
