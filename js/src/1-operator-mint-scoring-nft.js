import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import CreditScoringContract from "../../built-contracts/credit-scoring.json" assert { type: "json" };

const url = 'https://cardano-preprod.blockfrost.io/api/v0';
const key = 'preproducgQyCbIAtRpfRQGcs7DqBOjN97VPQtd';

const API = new Blockfrost.BlockFrostAPI({
  projectId: key,
  apiUrl: url
});

(async function test() {
  const api = await lucid.Lucid.new(
    new lucid.Blockfrost(url, key),
    "Preprod"
  );

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

  // Data Model

  let agePoint = 0;
  if (age >= 18 && age <= 25) {
    agePoint = 10;
  } else if (age >= 26 && age <= 40) {
    agePoint = 25;
  } else if (age >= 41 && age <= 65) {
    agePoint = 38;
  } else if (age >= 66) {
    agePoint = 43
  }

  let salaryPoint = 0;
  if (salary > 0 && salary <= 20000) {
    salaryPoint = -10;
  } else if (salary > 20000 && salary <= 40000) {
    salaryPoint = 16;
  } else if (salary > 4000 && salary <= 70000) {
    salaryPoint = 28;
  } else if (salary > 70000) {
    salaryPoint = 45
  }

  const pointOfFactors = [BigInt(agePoint), BigInt(salaryPoint)];

  const ageWeight = 50;
  const salaryWeight = 50;
  const weights = [BigInt(ageWeight), BigInt(salaryWeight)];

  // Redeemer

  const redeemer = lucid.Data.to(
    new lucid.Constr(0, [pointOfFactors, weights])
  );  

  // Datum 
  let totalScore = 0n;
  for (let i = 0; i < pointOfFactors.length; i++) {
    totalScore += pointOfFactors[i] * weights[i];
  }
  console.log(`totalScore: ${totalScore}`);
  // totalScore = 1000n;
  const owner = lucid.getAddressDetails(userAddress).paymentCredential?.hash || "";
  // const owner = "";
  const lendingPackage = 0n;

  let wallet = await api.selectWalletFromPrivateKey(paymentKey.to_bech32());
  let address = await wallet.wallet.address();

  let utxos = await api.utxosAt(address);
  console.log('utxos: ', utxos);

  let operatorUtxo = utxos.find(x => 
    x.assets[operatorId] == 1n
  );
  console.log('operatorUtxo: ', operatorUtxo);

  const mintingPolicy = {
    type: 'PlutusV2',
    script: lucid.applyParamsToScript(
      CreditScoringContract.cborHex,
      []
    )
  };

  const policyId = api.utils.mintingPolicyToId(
    mintingPolicy
  );
  // console.log('policyId: ', policyId);

  const unit = policyId + lucid.fromText(mintedNFTName);
  
  const infoObject = {};
  infoObject[mintedNFTName] = information;
  // console.log('infoObject: ', infoObject);

  const label = "721";
  const metadataNFT = {};
  metadataNFT[policyId] = infoObject;
  // console.log('metadata: ', metadataNFT);
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [totalScore, owner, lendingPackage])
  );

  const tx = await api.newTx()
  // .collectFrom([...utxos])
  .collectFrom([operatorUtxo])
  .mintAssets({ [unit]: 1n }, redeemer)
  .attachMintingPolicy(mintingPolicy)
  .attachMetadata(label, metadataNFT)
  .payToAddressWithData(userAddress, { inline: datum }, { [unit]: 1n })
  .complete();

  const signedTx = await tx.sign().complete();

  try {
    const txHash = await signedTx.submit();
    console.log('hash: ', txHash);
  } catch (error) {
    console.log('error: ', error);
  }
})().catch(error => {
  console.log('error: ', error);
});
