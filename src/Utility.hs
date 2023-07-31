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

module Utility
  (
    toCurrencySymbol,
    first,
    second,
    third,
    fourth
  )
where

import qualified Data.Aeson.Extras          as JSON
import qualified Data.Text                  as T
import qualified Plutus.Script.Utils.Value  as Value
import qualified Plutus.V2.Ledger.Api       as PlutusV2
import           PlutusTx.Prelude           as P hiding (Semigroup (..), unless, (.))
import           Prelude                    ((.))
import qualified Prelude                    as Haskell

-- This function is to convert from string to currency symbol
toCurrencySymbol :: Haskell.String -> PlutusV2.CurrencySymbol
toCurrencySymbol str = case (JSON.tryDecode . T.pack) str of
  Left  _ -> (Value.currencySymbol . fromBuiltin) emptyByteString
  Right b -> Value.currencySymbol b

first :: (Integer, Integer, Integer, Integer) -> Integer
first (x, _, _, _)  = x

second :: (Integer, Integer, Integer, Integer) -> Integer
second (_, x, _, _) = x

third :: (Integer, Integer, Integer, Integer) -> Integer
third (_, _, x, _)  = x

fourth :: (Integer, Integer, Integer, Integer) -> Integer
fourth (_, _, _, x) = x
