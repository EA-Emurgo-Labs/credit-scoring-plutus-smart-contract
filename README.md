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
cd ../credit-scoring
```

Build contract

```
cabal update
cabal build
```

## Create plutus script file

1. ManageScoringToken contract

```
cabal run manage <operatorTokenPolicy> <operatorTokenName> <minusPointsIfLatePayment>
```

For example:
```
cabal run manage c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 10
```

2. MintScoringToken contract

```
cabal run mint <operatorTokenPolicy> <operatorTokenName> <minScoreToMintScoringToken> <hash of ManageScoringToken contract>
```

Contract MintScoringToken depends on contract ManageScoringToken because after minting, the Scoring Token must be sent to ManageScoringToken contract only. So we need to run this script to get the hash of ManageScoringToken contract:

```
cd $HOME/credit-scoring/js
npm install
node src/utils/from-script-to-addr.js
```

The result, for example: 7b31444e6d184f034a934e4221743b8117cb63c74fccc2479aff7322

Then, the command to build MintScoringToken, for example:
```
cabal run mint c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 1000 7b31444e6d184f034a934e4221743b8117cb63c74fccc2479aff7322
```

## Interacte with contracts by using lucid-cardano

Go to js folder:
```
cd $HOME/credit-scoring/js
```

1. The operator mint a new Scoring Token for user

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

//-------------------------------------------------------------------------
```

Run file src/1-mint-scoring-nft.js:
```
node src/1-mint-scoring-nft.js
```

2. The operator update new base score for each Scoring Token at the beginning of each month

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

//-------------------------------------------------------------------------
```

Run file src/2-update-score.js:
```
node src/2-update-score.js
```
