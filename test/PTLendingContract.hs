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
import           LendingContract            as LC
import           PlutusTx.Prelude           (find, isJust)
-- import qualified Plutus.V2.Ledger.Api       as PlutusV2
-- import qualified Plutus.V2.Ledger.Contexts  as PlutusV2
-- import qualified PlutusTx

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

main :: IO ()
main = defaultMain $ do
  testGroup
    "Test lending contract"
    [ 
      testProperty "Borrow: user doesn't have Scoring NFT in inputs" prop_Borrow_NoScoringNFTInInputs_Fails
    ]

---------------------------------------------------------------------------------------------------
-------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

mock :: Mock
mock = initMock defaultBabbage (adaValue 100_000_000 <> operatorTokenValue)

instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp mock in property a

fakeOperatorToken :: FakeCoin
fakeOperatorToken = FakeCoin $ toBuiltinByteString $ "OperatorToken"

operatorTokenAsset :: AssetClass
operatorTokenAsset = fakeCoin(fakeOperatorToken)

operatorTokenValue :: Value
operatorTokenValue = fakeValue(fakeOperatorToken) 1

toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString = fromString

toBString :: String -> BS8.ByteString
toBString = fromString

nameScoringNFT :: TokenName
nameScoringNFT = tokenName . toBString $ "ScoringNFTEmurgoLabs"

operatorParams :: OperatorParams
operatorParams = OperatorParams {
  operatorToken = operatorTokenAsset,
  minScoreToMintNFT = 1000
}

mintingPolicy :: MintingPolicy
mintingPolicy = CS.policy operatorParams

mintingContract :: TypedPolicy redeemer
mintingContract = TypedPolicy $ toV2 mintingPolicy

lendingParams :: LendingParams
lendingParams = LendingParams {
  operatorToken' = operatorTokenAsset,
  scoringNFT = assetClass (CS.mintingContractSymbol operatorParams) nameScoringNFT,
  lendingPackagesInfo = [(1, 1000, 2000, 1000), (2, 2001, 3000, 2000), (3, 3001, 4000, 3000)]
}

scriptLendingContract :: TypedValidator datum redeemer
scriptLendingContract = TypedValidator $ toV2 $ LC.validator lendingParams

setupUsers :: Run [PubKeyHash]
setupUsers = do
  operator        <- newUser (adaValue 1000 <> operatorTokenValue)
  scoringNFTOwner <- newUser (adaValue 1000)
  normalUser      <- newUser (adaValue 1000)
  pure $ [operator, scoringNFTOwner, normalUser]

mintNFT :: UserSpend -> Value -> [Integer] -> [Integer] -> PubKeyHash -> PubKeyHash -> Value -> Tx
mintNFT usp valScoringNFT pointOfFactors weights user pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , mintValue mintingContract (pointOfFactors, weights) valScoringNFT
  , payToKeyDatum user (HashDatum (NFTInfo (getTotalScore pointOfFactors weights) user 0)) valScoringNFT
  , payToKey pkhOperator valOperatorToken
  ]

createLendingPackages :: UserSpend -> Integer -> Value -> PubKeyHash -> Value -> Tx
createLendingPackages usp packageNumber val pkhOperator valOperatorToken = mconcat
  [ userSpend usp
  , payToScript scriptLendingContract (HashDatum (packageNumber)) val
  , payToKey pkhOperator valOperatorToken
  ]

borrowPackage :: UserSpend -> PubKeyHash -> Integer -> Value -> Integer -> Value -> Tx
borrowPackage usp user packageNumber valPackage userScore valScoringNFT = mconcat
  [ userSpend usp
  , payToScript scriptLendingContract (HashDatum (NFTInfo userScore user packageNumber)) valScoringNFT
  , payToKey user valPackage
  ]

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

prop_Borrow_NoScoringNFTInInputs_Fails :: Bool -> Integer -> Property
prop_Borrow_NoScoringNFTInInputs_Fails isScoringNFTOwner userScore =
  (isScoringNFTOwner == False) ==> 
    runChecks False isScoringNFTOwner userScore

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

runChecks :: Bool -> Bool -> Integer -> Property
runChecks canBorrow isScoringNFTOwner userScore =
  collect (isScoringNFTOwner, userScore) $ monadic property check
  -- monadic property check
    where 
      check = do
        isGood <- run $ testValues canBorrow isScoringNFTOwner userScore
        assert isGood

testValues :: Bool -> Bool -> Integer -> Run Bool
testValues canBorrow isScoringNFTOwner userScore = do
  [operator, scoringNFTOwner, normalUser] <- setupUsers

  let normalVal     = adaValue 100 
      operatorVal   = operatorTokenValue
      valScoringNFT = singleton (CS.mintingContractSymbol operatorParams) nameScoringNFT 1
 
  uspOperator <- spend operator operatorVal
  let tx = mintNFT uspOperator valScoringNFT [10] [100] scoringNFTOwner operator operatorVal
  submitTx operator tx
  waitNSlots 10

  uspOperator1 <- spend operator operatorVal
  let package1 = 1
  let amount1 = adaValue 1000
  let tx1 = createLendingPackages uspOperator1 package1 amount1 operator operatorVal
  submitTx operator tx1
  waitNSlots 10

  uspOperator2 <- spend operator operatorVal
  let package2 = 2
  let amount2 = adaValue 2000
  let tx2 = createLendingPackages uspOperator2 package2 amount2 operator operatorVal
  submitTx operator tx2
  waitNSlots 10

  uspOperator3 <- spend operator operatorVal
  let package3 = 3
  let amount3 = adaValue 1000
  let tx3 = createLendingPackages uspOperator3 package3 amount3 operator operatorVal
  submitTx operator tx3
  waitNSlots 10

  uspScoringNFTOwner <- spend scoringNFTOwner valScoringNFT
  uspNormalUser      <- spend normalUser normalVal

  let packageNumber = 1
  let amount = adaValue 1000

  let usp  = if isScoringNFTOwner then uspScoringNFTOwner else uspNormalUser
      user = if isScoringNFTOwner then scoringNFTOwner else normalUser
      tx4  = borrowPackage usp user packageNumber amount userScore valScoringNFT

  if canBorrow then submitTx user tx4 else mustFail . submitTx user $ tx4

  waitNSlots 10

  utxos1 <- utxoAt user
  utxos2 <- utxoAt scriptLendingContract

  let valueSentToUser = find(\x -> do
                          let (_, txOut') = x
                              value' = txOutValue txOut'
                              [(_, _, amt)] = flattenValue value'
                          amt == 1000000000
                        ) utxos1

  let nftSentToContract = find(\x -> do
                            let (_, txOut') = x
                                value' = txOutValue txOut'
                                [(cs, tn, amt)] = flattenValue value'
                            cs == CS.mintingContractSymbol operatorParams && tn == nameScoringNFT && amt == 1
                          ) utxos2


  if canBorrow then
    return $ isJust valueSentToUser && isJust nftSentToContract
  else
    return $ not $ isJust valueSentToUser && isJust nftSentToContract
