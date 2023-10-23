# credit-scoring

## Description

This project is to calculate the credit scoring for user in our system, then mint a new Scoring Token with score attached in datum. After that, the Scoring Token will be used in other systems, for example: to borrow money from Lending contract, etc.

## Install the Haskell toolchain

Please follow this instruction: https://www.haskell.org/ghcup/

## Build contract

Clone this repo:
```
cd $HOME
https://github.com/EA-Emurgo-Labs/credit-scoring.git
```

Check out branch:
```
cd $HOME/credit-scoring
git checkout -f main
```

Clone plutus-apps repo:
```
cd $HOME
git clone https://github.com/input-output-hk/plutus-apps
```

Check out branch:
```
cd $HOME/plutus-apps
git checkout -f v1.2.0
```

Enter nix
```
nix develop
```

Go to credit-scoring repo
```
cd $HOME/credit-scoring
```

Build contract

```
cabal update
cabal build
```

## Install js dependencies

Go to js folder:
```
cd $HOME/credit-scoring/js
npm install
```

## Create plutus script file

Go to contract folder:
```
cd $HOME/credit-scoring
```

1. MintScoringToken contract

```
cabal run mint <operatorTokenPolicy> <operatorTokenName> <minScoreToMintScoringToken>
```

For example:
```
cabal run mint c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 1000
```

Then, get the scoring token's policy:
```
node js/src/utils/get-scoring-token-policyid.js
```

Result, for example: fa300e31f9048daa62d428b2529092efa3dc1bbd03ac1a946fa463a4 

2. ManageScoringToken contract

Get hash of operator address

```
node js/src/utils/from-addr-to-pkh.js
```

Result, for example: 1f09ff804264f4071b5dc9d623f3e68c41431a48ce6a5fa58e3af97c

Get hash of Lending contract

```
node js/src/utils/from-script-to-addr.js
```

Result, for example: 1864988ebad792eb191a99bd3a4b4ca3dcdb81e9393806db50f07557

Build ManageScoringToken contract

```
cabal run manage <operatorTokenPolicy> <operatorTokenName> <pubkey hash of operator address> <scoringTokenPolicy> <scoringTokenName> <hash of Lending contract> <biasPoints>
```

For example:
```
cabal run manage c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 1f09ff804264f4071b5dc9d623f3e68c41431a48ce6a5fa58e3af97c fa300e31f9048daa62d428b2529092efa3dc1bbd03ac1a946fa463a4 ScoringToken 1864988ebad792eb191a99bd3a4b4ca3dcdb81e9393806db50f07557 10
```

## Interacte with contracts by using lucid-cardano

Go to js folder:
```
cd $HOME/credit-scoring/js
```

1. Create reference script for MintScoringToken contract

```
node src/utils/create-mint-ref-script.js
```

Result, for example: 4c95bf79ea90c3a6c7b97c31d835688b88e0a13a881dcc0d49f71dd2172f62ff

2. Create reference script for ManageScoringToken contract

```
node src/utils/create-manage-ref-script.js
```

Result, for example: d66b92c452b1f896aa0f7cf636cabd0c0549c9904aeb1fc48a60c36561701778

3. The operator mint a new Scoring Token for user

Edit file src/1-mint-scoring-token.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Mnemonic of operator address
const mnemonic = 'people retreat property birth interest panic hedgehog install then around hill hard stumble chef auction effort alter sleep sock mango robust melt female caught';

// Account index associcated with mnemonic to generate this address
const accountIndex = 0;

// Operator token
const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

// The owner of Scoring Token
const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

// Token's name
const tokenName = "ScoringToken";

// Token's info
const information = {
  "description": "This is a Scoring Token of Emurgo Labs",
  "name": tokenName,
  "id": "1",
  "image": "ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw"
};

// Address contains minting reference script
const addressHasMintRefScript = "addr_test1qzqcdfglhu5dj5kr5lzndv8523m9rw52sjnyqrrdskdss884fc2ygj44zg7wgyypety42mps7rm0ry8n036upzg7yn3s203m2r" ;

// Tx id contains minting reference script
const txIdHasMintRefScript = "4c95bf79ea90c3a6c7b97c31d835688b88e0a13a881dcc0d49f71dd2172f62ff";

//-------------------------------------------------------------------------
```

Run file src/1-mint-scoring-nft.js:
```
node src/1-mint-scoring-nft.js
```

4. The operator update new base score for each Scoring Token at the beginning of each month

Edit file src/2-update-score.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Mnemonic of operator address
const mnemonic = 'people retreat property birth interest panic hedgehog install then around hill hard stumble chef auction effort alter sleep sock mango robust melt female caught';

// Account index associcated with mnemonic to generate this address
const accountIndex = 0;

// Operator token
const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

// Address to update score
const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

// The scoring token
const scoringToken = "fa300e31f9048daa62d428b2529092efa3dc1bbd03ac1a946fa463a453636f72696e67546f6b656e";

// Address contains manager reference script
const addressHasManageRefScript = "addr_test1qzqcdfglhu5dj5kr5lzndv8523m9rw52sjnyqrrdskdss884fc2ygj44zg7wgyypety42mps7rm0ry8n036upzg7yn3s203m2r";

// Tx id contains manager reference script
const txIdHasManageRefScript = "d66b92c452b1f896aa0f7cf636cabd0c0549c9904aeb1fc48a60c36561701778";

//-------------------------------------------------------------------------
```

Run file src/2-update-score.js:
```
node src/2-update-score.js
```

## Run property-based testing

1. MintScoringToken contract

```
cabal run test-mint
```

2. ManageScoringToken contract

```
cabal run test-manage
```
