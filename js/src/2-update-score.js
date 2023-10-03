import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import CreditScoringContract from "../../built-contracts/credit-scoring.json" assert { type: "json" };

const url = 'https://cardano-preprod.blockfrost.io/api/v0';
const key = 'preprodqAq6ClZlZrpqNJpkdUli9KFNQE3WrtMZ';

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

  // Mnemonic of operator address
  const mnemonic = 'people retreat property birth interest panic hedgehog install then around hill hard stumble chef auction effort alter sleep sock mango robust melt female caught';

  // Account index associcated with mnemonic to generate this address
  const accountIndex = 0;

  // Operator token
  const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

  // The NFT's owner
  const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

  // Minted NFT's name
  const mintedNFTName = "ScoringToken";

  // Minted NFT's information
  // const information = {
  //   "description": "This is a Scoring NFT of Emurgo Labs",
  //   "name": mintedNFTName,
  //   "id": "1",
  //   "image": "ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw"
  // };


  // Private key of operator address
  // const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cc6beca62a4436767a309bb61e7266434449b2a63f4f7a3b60bd5a726407f3f8", "hex"));

  // Operator token
  // const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

  // Minted NFT's name
  // const mintedNFTName = "ScoringNFTEmurgoLabs";

  // Minted NFT's information
  // const information = {
  //   "description": "This is the Scoring NFT has been issued from Emurgo Labs",
  //   "name": "ScoringNFTEmurgoLabs",
  //   "id": "1",
  //   "image": "ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw"
  // };

  // The Scoring NFT's owner
  // const userAddress = "addr_test1vzym9zs9h9w6yexdxys5kvehn7jw2yee3yy64t65vfg4hqswdtwh9";

  // User data (to calculate the score)
  const age = 50;
  const salary = 30000;

  //-------------------------------------------------------------------------

  // Data Model

  // Define points
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

  // Define weights
  const ageWeight = 50;
  const salaryWeight = 50;

  //-------------------------------------------------------------------------
  const contractScript = {
    type: "PlutusV2",
    script: CreditScoringContract.cborHex,
  };
  const contractAddress = api.utils.validatorToAddress(
    contractScript
  );
  console.log('contractAddress: ', contractAddress);

  const pointOfFactors = [BigInt(agePoint), BigInt(salaryPoint)];

  const weights = [BigInt(ageWeight), BigInt(salaryWeight)];

  // Redeemer

  const redeemer = lucid.Data.to(
    new lucid.Constr(1, [pointOfFactors, weights])
  );  

  // Datum 
  let totalScore = 0n;
  for (let i = 0; i < pointOfFactors.length; i++) {
    totalScore += pointOfFactors[i] * weights[i];
  }
  console.log(`totalScore: ${totalScore}`);
  // totalScore = 3000n;
  const ownerPKH = lucid.getAddressDetails(userAddress).paymentCredential?.hash || "";
  const ownerSH = lucid.getAddressDetails(userAddress).stakeCredential?.hash || "";
  // const owner = "";
  const lendingScore = 0n;
  const lendingFlag = 0n;

  const wallet = await api.selectWalletFromSeed(
    mnemonic,
    {
      addressType: "Base",
      accountIndex: accountIndex
    }
  );

  const operatorAddress = await wallet.wallet.address();
  console.log('operatorAddress: ', operatorAddress);

  const contractUtxo = await api.utxosAt(contractAddress);
  console.log('contractUtxo: ', contractUtxo);

  const utxos = await api.utxosAt(operatorAddress);
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
  
  // const infoObject = {};
  // infoObject[mintedNFTName] = information;
  // console.log('infoObject: ', infoObject);

  // const label = "721";
  // const metadataNFT = {};
  // metadataNFT[policyId] = infoObject;
  // console.log('metadata: ', metadataNFT);
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [ownerPKH, ownerSH, totalScore, lendingScore, lendingFlag])
  );

  

  const tx = await api.newTx()
  // .collectFrom([...utxos])
  .collectFrom([operatorUtxo, ...contractUtxo], redeemer)
  // .mintAssets({ [unit]: 1n }, redeemer)
  .attachSpendingValidator(mintingPolicy)
  // .attachMetadata(label, metadataNFT)
  .payToContract(contractAddress, { inline: datum }, { [unit]: 1n })
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
