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
import           MintScoringToken
import           GeneralParams
import           Utility

main :: IO ()
main = do
  -- Get the info of operator token, min score to mint the Scoring Token
  -- After minting, the Scoring Token will be sent to the ManageScoringToken contract (manager contract) only.
  [operatorTokenPolicy, operatorTokenName, minScore'] <- getArgs

  -- Construct params for contract MintScoringToken.
  let mintParams = MintParams {
    operatorToken = Value.AssetClass (toCurrencySymbol operatorTokenPolicy, (Value.TokenName . BC.toBuiltin . C.pack) operatorTokenName),
    minScore = Haskell.read minScore' :: Integer
  }

  let contract = "built-contracts/mint-score.json"
  
  -- Built the plutus script for contract MintScoringToken.
  result <- writeFileTextEnvelope contract Nothing $ buildMintingContract mintParams
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built contract MintScoringToken successfully at: " ++ contract
