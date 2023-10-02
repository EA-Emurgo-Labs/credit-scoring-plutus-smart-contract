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
import           CreditScoringContract      as CS

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
  testGroup
    "Test credit scoring contract"
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

-- Init for the operator params.
operatorParams :: OperatorParams
operatorParams = OperatorParams {
  operatorToken = emurgoToken,
  minScoreToMintNFT = 1000
}

-- Create the minting policy.
mintingPolicy :: MintingPolicy
mintingPolicy = CS.policy operatorParams

-- Create the minting contract.
mintingContract :: TypedPolicy redeemer
mintingContract = TypedPolicy $ toV2 mintingPolicy

-- Set up users.
setupUsers :: Run [PubKeyHash]
setupUsers = do
  operator    <- newUser (adaValue 1000 <> emurgoValue)
  notOperator <- newUser (adaValue 1000)
  user        <- newUser (adaValue 0)
  pure $ [operator, notOperator, user]

{-
The minting transaction will do the following actions:

1. Get the unspent output from the operator.

2. Mint a new Scoring NFT.

3. Send this NFT to the user address, also attach the datum.

4. Send the operator token back to the operator address, if we don't do this, the transaction will fail.
-}
mintingTx :: UserSpend -> Value -> [Integer] -> [Integer] -> Integer -> PubKeyHash -> PubKeyHash -> Value -> Tx
mintingTx usp valScoringNFT pointOfFactors weights outputScore user pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , mintValue mintingContract (pointOfFactors, weights) valScoringNFT
  , payToKeyDatum user (HashDatum (NFTInfo outputScore user 0)) valScoringNFT
  , payToKey pkhOperator valOperatorToken
  ]

-- Convert from String to BuiltinByteString
toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString = fromString

-- Convert from String to BS8.ByteString
toBString :: String -> BS8.ByteString
toBString = fromString

-- Create the NFT's name
nameScoringNFT :: TokenName
nameScoringNFT = tokenName . toBString $ "ScoringNFTEmurgoLabs"

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

-- Test in case I'm not the operator, and the remaining values are random.
prop_NotOperator_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_NotOperator_Fails isOperator mintedAmount pointOfFactors weights outputScore =
  (isOperator == False) ==> 
    runChecks False isOperator mintedAmount pointOfFactors weights outputScore

-- Test in case I'm the operator, minted amount is greater than 1, and the remaining values are random.
prop_WrongMintedAmount_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_WrongMintedAmount_Fails isOperator mintedAmount pointOfFactors weights outputScore =
  (isOperator == True && mintedAmount > 1) ==>
    runChecks False isOperator mintedAmount pointOfFactors weights outputScore

-- Test in case I'm the operator, minted amount is 1, but poor score.
prop_PoorScore_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_PoorScore_Fails _ _ pointOfFactors _ outputScore =
  (length pointOfFactors >= 1 && pointOfFactors!!0 < 10) ==>
    runChecks False True 1 pointOfFactors [100] outputScore

-- Test in case I'm the operator, minted amount is 1, good score, but wrong output datum.
prop_WrongDatum_Fails :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_WrongDatum_Fails _ _ _ _ outputScore =
  (outputScore < 0) ==>
    runChecks False True 1 [10] [100] outputScore

-- Test in case I'm the operator, minted amount is 1, good score and output datum is correct (everything is good).
prop_AllGood_Succeeds :: Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
prop_AllGood_Succeeds _ _ pointOfFactors _ _ =
  (length pointOfFactors >= 1 && pointOfFactors!!0 >= 10) ==>
    runChecks True True 1 pointOfFactors [100] (getTotalScore pointOfFactors [100])

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

runChecks :: Bool -> Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Property
runChecks shouldMint isOperator mintedAmount pointOfFactors weights outputScore =
  collect (isOperator, mintedAmount, pointOfFactors, weights, outputScore) $ monadic property check
  -- monadic property check
    where 
      check = do
        isGood <- run $ testValues shouldMint isOperator mintedAmount pointOfFactors weights outputScore
        assert isGood

testValues :: Bool -> Bool -> Integer -> [Integer] -> [Integer] -> Integer -> Run Bool
testValues shouldMint isOperator mintedAmount pointOfFactors weights outputScore = do
  -- Setup 3 users.
  [operator, notOperator, user] <- setupUsers

  -- Create some variables.
  let normalVal     = adaValue 100 
      operatorVal   = emurgoValue
      valScoringNFT = singleton (CS.symbolNFT operatorParams) nameScoringNFT mintedAmount
      creator       = if isOperator then operator else notOperator
 
  -- Get user spent of the operator.
  uspOperator <- spend operator operatorVal

  -- Get user spent of not the operator.
  uspAnother  <- spend notOperator normalVal

  -- Get actual user spent and make transaction to mint NFT and pay back the operator token to operator.
  let usp = if isOperator then uspOperator else uspAnother
      tx  = mintingTx usp valScoringNFT pointOfFactors weights outputScore user operator operatorVal

  -- Submit transaction.
  if shouldMint then submitTx creator tx else mustFail . submitTx creator $ tx

  -- Wait a little bit after submitting a transaction.
  waitNSlots 10

  -- Get utxo of user address.
  utxos <- utxoAt user

  -- Verify the NFT has been sent to the user address or not.
  if shouldMint then do
    let [(_, oOut)] = utxos
        [(cs, tn, amount)] = flattenValue $ txOutValue oOut
    return $ cs == CS.symbolNFT operatorParams && tn == nameScoringNFT && amount == 1
  else
    return $ length utxos == 0
