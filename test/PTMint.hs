{-# LANGUAGE    NumericUnderscores #-}
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
import           Utility
import           MintScoringToken           as Mint
import           ManageScoringToken         as Manager
import qualified Plutus.Model               as Model

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
  testGroup
    "Test MintScoringToken contract"
    [ 
      testProperty "Is NOT operator"                                                      prop_NotOperator_Fails
    , testProperty "Is operator, but wrong minted amount"                                 prop_WrongMintedAmount_Fails
    , testProperty "Is operator, right minted amount, but poor score"                     prop_PoorScore_Fails
    , testProperty "Is operator, right minted amount, good score, but wrong output datum" prop_WrongDatum_Fails
    , testProperty "Everything is correct"                                                prop_AllGood_Succeeds
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

-- Init for the mint params.
mintParams :: MintParams
mintParams = MintParams {
  operatorToken = emurgoToken,
  minScore = 1000
}

-- Create the minting policy.
mintingPolicy :: MintingPolicy
mintingPolicy = Mint.policy mintParams

-- Create the minting contract.
mintingContract :: TypedPolicy redeemer
mintingContract = TypedPolicy $ toV2 mintingPolicy

-- Init for the manage params.
manageParams :: ManageParams
manageParams = ManageParams {
  operatorToken' = emurgoToken,
  operatorAddr = PubKeyHash $ toBuiltinByteString "1f09ff804264f4071b5dc9d623f3e68c41431a48ce6a5fa58e3af97c",
  revenueAddr = PubKeyHash $ toBuiltinByteString "1f09ff804264f4071b5dc9d623f3e68c41431a48ce6a5fa58e3af97c",
  scoringToken = assetClass (Mint.tokenSymbol mintParams) nameScoringToken,
  lendingContract = Model.toValidatorHash mintingContract,
  biasPoints = 10
}

-- Create manager script
managerContractAddress :: TypedValidator datum redeemer
managerContractAddress = TypedValidator $ toV2 $ Manager.validator manageParams

-- Set up users.
setupUsers :: Run [PubKeyHash]
setupUsers = do
  operator    <- newUser (adaValue 1000 <> emurgoValue)
  notOperator <- newUser (adaValue 1000)
  pure $ [operator, notOperator]

-- This transaction is to mint a new Scoring Token.
mintingTx :: UserSpend -> Value -> [Integer] -> [Integer] -> Integer -> PubKeyHash -> Value -> Tx
mintingTx usp valScoringToken pointsOfFactors' weights' outputBaseScore pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , mintValue mintingContract (pointsOfFactors', weights' ) valScoringToken
  , payToScript managerContractAddress (InlineDatum (ScoringTokenInfo (PubKeyHash $ toBuiltinByteString "1ff74582a0eabea2d3f8778539fe4fb31c6249cd0bf523ed386e026c") (ScriptHash $ toBuiltinByteString "ebf242a5cde8e4e43fc188a8104183d65706257b578692291ff80262") outputBaseScore 0 0 0)) valScoringToken
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

-- Test in case I'm not the operator, and the remaining values are random.
prop_NotOperator_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_NotOperator_Fails isOperator mintedAmount pointsOfFactors' weights' outputBaseScore =
  (isOperator == False) ==> 
    runChecks False isOperator mintedAmount pointsOfFactors' weights' outputBaseScore

-- Test in case I'm the operator, minted amount is greater than 1, and the remaining values are random.
prop_WrongMintedAmount_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_WrongMintedAmount_Fails isOperator mintedAmount pointsOfFactors' weights' outputBaseScore =
  (isOperator == True && mintedAmount > 1) ==>
    runChecks False isOperator mintedAmount pointsOfFactors' weights' outputBaseScore

-- Test in case I'm the operator, minted amount is 1, but poor score.
prop_PoorScore_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_PoorScore_Fails _ _ pointsOfFactors' _ outputBaseScore =
  (length pointsOfFactors' >= 1 && pointsOfFactors'!!0 < 10) ==>
    runChecks False True 1 pointsOfFactors' [100] outputBaseScore

-- Test in case I'm the operator, minted amount is 1, good score, but wrong output datum.
prop_WrongDatum_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_WrongDatum_Fails _ _ _ _ outputBaseScore =
  (outputBaseScore < 0) ==>
    runChecks False True 1 [10] [100] outputBaseScore

-- Test in case I'm the operator, minted amount is 1, good score and output datum is correct (everything is good).
prop_AllGood_Succeeds :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_AllGood_Succeeds _ _ pointsOfFactors' _ _ =
  (length pointsOfFactors' >= 1 && pointsOfFactors'!!0 >= 10) ==>
    runChecks True True 1 pointsOfFactors' [100] (getBaseScore pointsOfFactors' [100])

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

runChecks :: Bool -> Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
runChecks shouldMint isOperator mintedAmount pointsOfFactors' weights' outputBaseScore =
  collect (isOperator, mintedAmount, pointsOfFactors', weights', outputBaseScore) $ monadic property check
  -- monadic property check
    where 
      check = do
        isGood <- run $ testValues shouldMint isOperator mintedAmount pointsOfFactors' weights' outputBaseScore
        assert isGood

testValues :: Bool -> Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Run Bool
testValues shouldMint isOperator mintedAmount pointsOfFactors' weights' outputBaseScore = do
  -- Setup 2 users.
  [operator, notOperator] <- setupUsers

  -- Create some variables.
  let normalVal       = adaValue 100 
      operatorVal     = emurgoValue
      valScoringToken = singleton (Mint.tokenSymbol mintParams) nameScoringToken mintedAmount
      creator         = if isOperator then operator else notOperator
 
  -- Get user spent of the operator.
  uspOperator <- spend operator operatorVal

  -- Get user spent of not the operator.
  uspAnother  <- spend notOperator normalVal

  -- Get actual user spent and make transaction to mint Scoring Token and pay back the operator token to operator.
  let usp = if isOperator then uspOperator else uspAnother
      tx  = mintingTx usp valScoringToken pointsOfFactors' weights' outputBaseScore operator operatorVal

  -- Submit transaction.
  if shouldMint then submitTx creator tx else mustFail . submitTx creator $ tx

  -- Wait a little bit after submitting a transaction.
  waitNSlots 10

  -- Get utxo of ManageScoringToken contract.
  utxos <- utxoAt managerContractAddress

  -- Verify the Scoring Token has been sent to the ManageScoringToken contract or not.
  if shouldMint then do
    let [(_, oOut)] = utxos
        [(cs, tn, amt)] = flattenValue $ txOutValue oOut
    return $ cs == Mint.tokenSymbol mintParams && tn == nameScoringToken && amt == 1
  else
    return $ length utxos == 0
