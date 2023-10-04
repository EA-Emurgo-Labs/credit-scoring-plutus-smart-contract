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
import qualified Data.ByteString.Char8      as C
import qualified Plutus.Script.Utils.Value  as Value
import qualified PlutusTx.Builtins.Class    as BC
import           PlutusTx.Prelude           as P hiding ((.))
import           Prelude                    (IO, (.))
import qualified Prelude                    as Haskell
import           System.Environment
import           ManageScoringToken
import           GeneralParams
import           Utility

main :: IO ()
main = do
  -- Get the info of operator token, minus points (in case of late payment in lending)
  -- to manage the Scoring Token.
  [tokenPolicy, tokenName, minusPoints] <- getArgs

  -- Construct params for contract ManageScoringToken.
  let manageParams = ManageParams {
    operatorToken' = Value.AssetClass (toCurrencySymbol tokenPolicy, (Value.TokenName . BC.toBuiltin . C.pack) tokenName),
    minusPointsIfLatePayment = Haskell.read minusPoints :: Integer
  }

  let contract = "built-contracts/manage-score.json"
  
  -- Built the plutus script for contract ManageScoringToken.
  result <- writeFileTextEnvelope contract Nothing $ buildManagerContract manageParams
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built contract ManageScoringToken successfully at: " ++ contract
