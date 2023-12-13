{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- import MintScoringToken
import ManageScoringToken
import Prelude             (IO)

-- This is the main function for parameterized contracts
main :: IO ()
main = do
  -- MintScoringToken.saveMintCode
  ManageScoringToken.saveManageCode
