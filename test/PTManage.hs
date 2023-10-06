{-# LANGUAGE    NumericUnderscores #-}
{-# LANGUAGE    TypeApplications   #-}
{-# OPTIONS_GHC -Wno-orphans       #-}

module Main where

import           Plutus.Model
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Data.String
import           PlutusTx.Builtins
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import qualified Data.ByteString.Char8      as BS8
import           GeneralParams
import           MintScoringToken           as Mint
import           ManageScoringToken         as Manager
import qualified Plutus.Model               as Model

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
  testGroup
    "Test ManageScoringToken contract"
    [ 
      testProperty "Is NOT operator" prop_NotOperator_Fails
    , testProperty "Is operator"     prop_AllGood_Succeeds
    ]

---------------------------------------------------------------------------------------------------
-------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

-- Init a new blockchain with 10,000,000 ADA and 1 Emurgo token (the operator token).
mock :: Mock
mock = initMock defaultBabbage (adaValue 10_000_000 <> emurgoValue)

instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp mock in property a

-- Create a fake coin, it will be used to create the operator token.
fake :: FakeCoin
fake = FakeCoin $ toBuiltinByteString $ "Emurgo"

-- Create the operator token, this token is used to verify who is the operator.
emurgoToken :: AssetClass
emurgoToken = fakeCoin(fake)

-- Init the value for operator token (just one).
emurgoValue :: Value
emurgoValue = fakeValue(fake) 1

-- Init for the manage params.
manageParams :: ManageParams
manageParams = ManageParams {
  operatorToken' = emurgoToken,
  minusPointsIfLatePayment = 10
}

-- Create manager script
managerContractAddress :: TypedValidator datum redeemer
managerContractAddress = TypedValidator $ toV2 $ Manager.validator manageParams

-- Init for the mint params.
mintParams :: MintParams
mintParams = MintParams {
  operatorToken = emurgoToken,
  minScoreToMintScoringToken = 1000,
  managerContract = Model.toValidatorHash managerContractAddress
}

-- Create the minting policy.
mintingPolicy :: MintingPolicy
mintingPolicy = Mint.policy mintParams

-- Create the minting contract.
mintingContract :: TypedPolicy redeemer
mintingContract = TypedPolicy $ toV2 mintingPolicy

-- Set up users.
setupUsers :: Run [PubKeyHash]
setupUsers = do
  operator    <- newUser (adaValue 1000 <> emurgoValue)
  notOperator <- newUser (adaValue 1000)
  pure $ [operator, notOperator]

-- This transaction is to mint a new Scoring Token.
mintScoringTokenTx :: UserSpend -> Value -> [Integer] -> [Integer] -> Integer -> PubKeyHash -> Value -> Tx
mintScoringTokenTx usp valScoringToken pointsOfFactors' weights' outputBaseScore pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , mintValue mintingContract (pointsOfFactors', weights' ) valScoringToken
  , payToScript managerContractAddress (InlineDatum (TokenInfo (PubKeyHash $ toBuiltinByteString "1ff74582a0eabea2d3f8778539fe4fb31c6249cd0bf523ed386e026c") (ScriptHash $ toBuiltinByteString "ebf242a5cde8e4e43fc188a8104183d65706257b578692291ff80262") outputBaseScore 0 0 0)) valScoringToken
  , payToKey pkhOperator valOperatorToken
  ]

-- This transaction is to update score.
updateScoreTx :: UserSpend -> Value -> TxOutRef -> TokenInfo -> [Integer] -> [Integer] -> Integer -> PubKeyHash -> Value -> Tx
updateScoreTx usp valScoringToken utxoContract oldDatum pointsOfFactors' weights' outputBaseScore pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , spendScript managerContractAddress utxoContract (pointsOfFactors', weights') oldDatum
  , payToScript managerContractAddress (InlineDatum (TokenInfo (PubKeyHash $ toBuiltinByteString "1ff74582a0eabea2d3f8778539fe4fb31c6249cd0bf523ed386e026c") (ScriptHash $ toBuiltinByteString "ebf242a5cde8e4e43fc188a8104183d65706257b578692291ff80262") outputBaseScore 0 0 0)) valScoringToken
  , payToKey pkhOperator valOperatorToken
  ]

-- Convert from String to BuiltinByteString
toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString = fromString

-- Convert from String to BS8.ByteString
toBString :: String -> BS8.ByteString
toBString = fromString

-- Create the Token's name
nameScoringToken:: TokenName
nameScoringToken = tokenName . toBString $ "ScoringToken"

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

-- Test in case I'm not the operator.
prop_NotOperator_Fails :: Bool -> Property
prop_NotOperator_Fails isOperator =
  (isOperator == False) ==> 
    runChecks False isOperator

-- Test in case I'm the operator.
prop_AllGood_Succeeds :: Bool -> Property
prop_AllGood_Succeeds isOperator =
  (isOperator == True) ==>
    runChecks True isOperator

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

runChecks :: Bool -> Bool -> Property
runChecks isSuccess' isOperator =
  collect (isOperator) $ monadic property check
  -- monadic property check
    where 
      check = do
        isGood <- run $ testValues isSuccess' isOperator
        assert isGood

testValues :: Bool -> Bool -> Run Bool
testValues isSuccess' isOperator = do
  -- Setup 2 users.
  [operator, notOperator] <- setupUsers

  -- Create some variables.
  let normalVal       = adaValue 100 
      operatorVal     = emurgoValue
      valScoringToken = singleton (Mint.tokenSymbol mintParams) nameScoringToken 1
 
  -- Get user spent of the operator.
  uspOperator <- spend operator operatorVal

  -- Creat a transaction to mint a new Scoring Token.
  let tx = mintScoringTokenTx uspOperator valScoringToken [10] [100] 1000 operator operatorVal

  -- Submit tx
  submitTx operator tx

  -- Wait a little bit after submitting a transaction.
  waitNSlots 10

  -- Get utxo and old datum of ManageScoringToken contract.
  [(txOutRefContract, _)] <- utxoAt managerContractAddress
  oldDatum <- datumAt @TokenInfo txOutRefContract

  case oldDatum of
    Just i -> do
      -- Get user spent of the operator.
      uspOperator' <- spend operator operatorVal

      -- Get user spent of not the operator.
      uspAnother'  <- spend notOperator normalVal

      -- Create some variables.
      let creator = if isOperator then operator else notOperator
          usp     = if isOperator then uspOperator' else uspAnother'
          tx'     = updateScoreTx usp valScoringToken txOutRefContract i [20] [100] 2000 operator operatorVal

      -- Submit tx
      if isSuccess' then submitTx creator tx' else mustFail . submitTx creator $ tx'

      -- Wait a little bit after submitting a transaction.
      waitNSlots 10

      -- Get new datum
      [(txOutRefContract', _)] <- utxoAt managerContractAddress
      datum <- datumAt @TokenInfo txOutRefContract'

      -- Verify the new base score in new datum.
      if isSuccess' then do
        case datum of
          Just (TokenInfo _ _ newBaseScore _ _ _) -> return $ newBaseScore == 2000
          Nothing -> return False
      else do
        case datum of
          Just (TokenInfo _ _ newBaseScore _ _ _) -> return $ newBaseScore == 1000
          Nothing -> return False

    Nothing -> return False
