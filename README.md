# credit-scoring

## Description

This project is to calculate the credit scoring for user in our system, then mint a new Scoring NFT for user with score attached in datum. After that, the scoring will be used in other systems, for example: to borrow money from Lending contract, etc.

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
git checkout -f cong-dev
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

cabal run manage c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 10

cabal run mint c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 1000 b0596dc9bf3c96e339178dda811966071d7d550599dc304c23b57e5e

## Create plutus script file

1. Credit Scoring contract

```
cabal run credit-scoring <operatorTokenPolicy> <operatorTokenName> <minScoreToMintNFT>
```

For example:
```
cabal run credit-scoring c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 1000
```

2. Lending contract

```
cabal run lending <operatorTokenPolicy> <operatorTokenName> <scoringNFTPolicy> <scoringNFTName> <one or more lendingPackagesInfo (including: packageNumber, fromPoint, toPoint, lendingAmount)>
```

For example:
```
cabal run lending c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f12 MoonstakeTestnet1 d9a6c3ebf6492749f6063a1851d6607c1c8ce289da762c11a19bd322 ScoringNFTEmurgoLabs 1 1000 2000 1000 2 2001 3000 2000 3 3001 4000 3000
```

## Interacte with contracts by using lucid-cardano

Go to js folder:
```
cd $HOME/credit-scoring/js
```

1. The operator mint a new Scoring NFT for user

Edit file src/1-operator-mint-scoring-nft.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Private key of operator address
const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cc6beca62a4436767a309bb61e7266434449b2a63f4f7a3b60bd5a726407f3f8", "hex"));

// Operator token
const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

// Minted NFT's name
const mintedNFTName = "ScoringNFTEmurgoLabs";

// Minted NFT's information
const information = {
  "description": "This is the Scoring NFT has been issued from Emurgo Labs",
  "name": "ScoringNFTEmurgoLabs",
  "id": "1",
  "image": "ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw"
};

// The Scoring NFT's owner
const userAddress = "addr_test1vzym9zs9h9w6yexdxys5kvehn7jw2yee3yy64t65vfg4hqswdtwh9";

// User data (to calculate the score)
const age = 30;
const salary = 30000;

//-------------------------------------------------------------------------
```

Run file src/1-operator-mint-scoring-nft.js:
```
node src/1-operator-mint-scoring-nft.js
```

2.1 The operator sends funds to Lending contract to create some lending packages

Edit file src/2.1-operator-create-lending-packages.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Private key of operator address
const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cc6beca62a4436767a309bb61e7266434449b2a63f4f7a3b60bd5a726407f3f8", "hex"));

// Operator token
const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

// Lending package number:
//  + packageNumber = 1: 1000 ADA
//  + packageNumber = 2: 2000 ADA
//  + packageNumber = 3: 3000 ADA
const packageNumber = 1;

//-------------------------------------------------------------------------
```

Run file src/2.1-operator-create-lending-packages.js:
```
node src/2.1-operator-create-lending-packages.js
```

2.2 User borrow money from Lending contract

Edit file src/2.2-user-borrow.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Private key of user address
const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));

// Scoring NFT
const scoringNFT = "29945394e952ab8b98d0619a7bb1ec186045cfd1155a1dee50eed0ef53636f72696e674e4654456d7572676f4c616273";

// Lending package number that user want to borrow:
//  + packageNumber = 1: 1000 ADA (score 1000 -> 2000 points)
//  + packageNumber = 2: 2000 ADA (score 2001 -> 3000 points)
//  + packageNumber = 3: 3000 ADA (score 3001 -> 4000 points)
const packageNumber = 1;

//-------------------------------------------------------------------------
```

Run file src/2.2-user-borrow.js:
```
node src/2.2-user-borrow.js
```

2.3 User pay back money

Edit file src/2.3-user-payback.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Private key of user address
const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));

// Scoring NFT
const scoringNFT = "29945394e952ab8b98d0619a7bb1ec186045cfd1155a1dee50eed0ef53636f72696e674e4654456d7572676f4c616273";

//-------------------------------------------------------------------------
```

Run file src/2.3-user-payback.js:
```
node src/2.3-user-payback.js
```

2.4 The operator claims back their funds

Edit file src/2.4-operator-claim-funds.js with some fields:

```
//-------------------------------------------------------------------------
// PLEASE CHANGE SOME VARIABLES HERE IN YOUR CASE

// Private key of operator address
const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cc6beca62a4436767a309bb61e7266434449b2a63f4f7a3b60bd5a726407f3f8", "hex"));

// Operator token
const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

// Scoring NFT
const scoringNFT = "29945394e952ab8b98d0619a7bb1ec186045cfd1155a1dee50eed0ef53636f72696e674e4654456d7572676f4c616273";

//-------------------------------------------------------------------------
```

Run file src/2.4-operator-claim-funds.js:
```
node src/2.4-operator-claim-funds.js
```
