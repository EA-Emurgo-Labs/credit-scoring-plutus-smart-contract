import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import mintContract from "../../built-contracts/mint-score.json" assert { type: "json" };
import manageContract from "../../built-contracts/manage-score.json" assert { type: "json" };

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

  // The owner of Scoring Token
  const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

  // Minted NFT's name
  const tokenName = "ScoringToken";

  // Minted NFT's information
  const information = {
    "description": "This is a Scoring Token of Emurgo Labs",
    "name": tokenName,
    "id": "1",
    "image": "ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw"
  };

  //-------------------------------------------------------------------------

  // Data Model

  // Define points
  // let agePoint = 0;
  // if (age >= 18 && age <= 25) {
  //   agePoint = 10;
  // } else if (age >= 26 && age <= 40) {
  //   agePoint = 25;
  // } else if (age >= 41 && age <= 65) {
  //   agePoint = 38;
  // } else if (age >= 66) {
  //   agePoint = 43
  // }

  // let salaryPoint = 0;
  // if (salary > 0 && salary <= 20000) {
  //   salaryPoint = -10;
  // } else if (salary > 20000 && salary <= 40000) {
  //   salaryPoint = 16;
  // } else if (salary > 4000 && salary <= 70000) {
  //   salaryPoint = 28;
  // } else if (salary > 70000) {
  //   salaryPoint = 45
  // }

  // Define weights
  // const ageWeight = 50;
  // const salaryWeight = 50;

  //-------------------------------------------------------------------------

  const pointsOfFactors = [10n, 10n];

  const weights = [50n, 50n];

  // Redeemer
  const redeemer = lucid.Data.to(
    new lucid.Constr(0, [pointsOfFactors, weights])
  );  

  // Datum 
  let baseScore = 0n;
  for (let i = 0; i < pointsOfFactors.length; i++) {
    baseScore += pointsOfFactors[i] * weights[i];
  }
  console.log(`baseScore: ${baseScore}`);
  // baseScore = 3000n;
  const ownerPKH = lucid.getAddressDetails(userAddress).paymentCredential?.hash || "";
  const ownerSH = lucid.getAddressDetails(userAddress).stakeCredential?.hash || "";
  // const owner = "";
  const lendingScore = 0n;
  const lendingPackage = 0n;
  const deadlinePayback = 0n;

  const wallet = await api.selectWalletFromSeed(
    mnemonic,
    {
      addressType: "Base",
      accountIndex: accountIndex
    }
  );

  const operatorAddress = await wallet.wallet.address();
  console.log('operatorAddress: ', operatorAddress);

  const utxos = await api.utxosAt(operatorAddress);
  console.log('utxos: ', utxos);

  let operatorUtxo = utxos.find(x => 
    x.assets[operatorId] == 1n
  );
  console.log('operatorUtxo: ', operatorUtxo);

  const mintingPolicy = {
    type: 'PlutusV2',
    script: lucid.applyParamsToScript(
      mintContract.cborHex,
      []
    )
  };

  const policyId = api.utils.mintingPolicyToId(
    mintingPolicy
  );
  // console.log('policyId: ', policyId);

  const unit = policyId + lucid.fromText(tokenName);
  
  const infoObject = {};
  infoObject[tokenName] = information;
  // console.log('infoObject: ', infoObject);

  const label = "721";
  const metadataToken = {};
  metadataToken[policyId] = infoObject;
  // console.log('metadata: ', metadataNFT);
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [ownerPKH, ownerSH, baseScore, lendingScore, lendingPackage, deadlinePayback])
  );

  const manageContractScript = {
    type: "PlutusV2",
    script: manageContract.cborHex,
  };
  const manageContractAddress = api.utils.validatorToAddress(
    manageContractScript
  );
  console.log('manageContractAddress: ', manageContractAddress);

  const tx = await api.newTx()
  // .collectFrom([...utxos])
  .collectFrom([operatorUtxo])
  .mintAssets({ [unit]: 1n }, redeemer)
  .attachMintingPolicy(mintingPolicy)
  .attachMetadata(label, metadataToken)
  // .payToContract(userAddress, { inline: datum }, { [unit]: 1n })
  // .payToContract(manageContractAddress, { inline: lucid.Data.void() }, { [unit]: 1n })
  .payToContract(manageContractAddress, { inline: datum }, { [unit]: 1n })
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
