import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import { collectUserInfo } from "./utils/collect-user-info.js";
import mintContract from "../../built-contracts/mint-score.json" assert { type: "json" };
import managerContract from "../../built-contracts/manage-score.json" assert { type: "json" };

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

  const managerContractScript = {
    type: "PlutusV2",
    script: managerContract.cborHex,
  };
  const managerContractAddress = api.utils.validatorToAddress(
    managerContractScript
  );
  console.log('managerContractAddress: ', managerContractAddress);

  const ownerPKH = lucid.getAddressDetails(userAddress).paymentCredential?.hash || "";
  console.log('ownerPKH: ', ownerPKH);
  const ownerSH = lucid.getAddressDetails(userAddress).stakeCredential?.hash || "";
  console.log('ownerSH: ', ownerSH);

  const contractUtxos = await api.utxosAt(managerContractAddress);

  let ownerPKHInDatum = "";
  let ownerSHInDatum = "";
  for (const utxo of contractUtxos) {
    const txInfo = await API.txsUtxos(utxo.txHash);

    let contractOutput = null;
    for (const output of txInfo.outputs) {
      if (output.address == managerContractAddress) {
        contractOutput = output;
        break;
      }
    }

    if (contractOutput != null) {
      console.log('contractOutput: ', contractOutput);

      const previousDatumHash = contractOutput.data_hash;

      if (previousDatumHash != null) {
        const previousDatum = await API.scriptsDatum(previousDatumHash);
        console.log('previousDatum: ', JSON.stringify(previousDatum, 0, 4));

        ownerPKHInDatum = previousDatum.json_value.fields[0]["bytes"];

        ownerSHInDatum = previousDatum.json_value.fields[1]["bytes"];

        if (ownerPKHInDatum == ownerPKH && ownerSHInDatum == ownerSH) {
          console.log("The scoring token has been minted for this user already!");
          return;
        }
      }
    }
  }

  //-------------------------------------------------------------------------

  // Collect user's data

  const userData = await collectUserInfo(userAddress);
  console.log('userData: ', userData);

  const addressBalance = userData.addressBalance;
  const stakingReward = userData.stakingReward;
  const numberTxs = userData.numberTxs;
  const totalSent = userData.totalSent;

  //-------------------------------------------------------------------------

  // Data Model

  // Define points

  let pointsOfFactor0 = 0n;
  if (addressBalance >= 0 && addressBalance < 100) {
    pointsOfFactor0 = 0n;
  } else if (addressBalance >= 100 && addressBalance < 1000) {
    pointsOfFactor0 = 10n;
  } else if (addressBalance >= 1000 && addressBalance < 10000) {
    pointsOfFactor0 = 20n;
  } else if (addressBalance >= 10000 && addressBalance < 100000) {
    pointsOfFactor0 = 30n;
  } else if (addressBalance > 100000) {
    pointsOfFactor0 = 50n;
  }

  let pointsOfFactor1 = 0n;
  if (stakingReward >= 0 && stakingReward < 1) {
    pointsOfFactor1 = 0n;
  } else if (stakingReward >= 1 && stakingReward < 10) {
    pointsOfFactor1 = 10n;
  } else if (stakingReward >= 10 && stakingReward < 100) {
    pointsOfFactor1 = 20n;
  } else if (stakingReward >= 100 && stakingReward < 1000) {
    pointsOfFactor1 = 30n;
  } else if (stakingReward > 1000) {
    pointsOfFactor1 = 50n;
  }

  let pointsOfFactor2 = 0n;
  if (numberTxs > 0 && numberTxs < 10) {
    pointsOfFactor2 = 10n;
  } else if (numberTxs >= 10 && numberTxs < 50) {
    pointsOfFactor2 = 50n;
  } else if (numberTxs >= 50 && numberTxs < 100) {
    pointsOfFactor2 = 30n;
  } else if (numberTxs >= 100 && numberTxs < 1000) {
    pointsOfFactor2 = 20n;
  } else if (numberTxs == 0 || numberTxs >= 1000) {
    pointsOfFactor2 = 0n;
  }

  let pointsOfFactor3 = 0;
  if (totalSent >= 0 && totalSent < 100) {
    pointsOfFactor3 = 0n;
  } else if (totalSent >= 100 && totalSent < 1000) {
    pointsOfFactor3 = 10n;
  } else if (totalSent >= 1000 && totalSent < 10000) {
    pointsOfFactor3 = 20n;
  } else if (totalSent >= 10000 && totalSent < 100000) {
    pointsOfFactor3 = 30n;
  } else if (totalSent > 100000) {
    pointsOfFactor3 = 50n;
  }

  // Define weights

  const weightOfFactor0 = 25n;
  const weightOfFactor1 = 25n;
  const weightOfFactor2 = 25n;
  const weightOfFactor3 = 25n;

  //-------------------------------------------------------------------------

  const pointsOfFactors = [pointsOfFactor0, pointsOfFactor1, pointsOfFactor2, pointsOfFactor3];

  const weights = [weightOfFactor0, weightOfFactor1, weightOfFactor2, weightOfFactor3];

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
  const lendingScore = 0n;
  const lendingAmount = 0n;
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

  const unit = policyId + lucid.fromText(tokenName);

  const infoObject = {};
  infoObject[tokenName] = information;
  const label = "721";
  const metadataToken = {};
  metadataToken[policyId] = infoObject;
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [ownerPKH, ownerSH, baseScore, lendingScore, lendingAmount, deadlinePayback])
  );

  // const addressHasRefScripts = "addr_test1qzqcdfglhu5dj5kr5lzndv8523m9rw52sjnyqrrdskdss884fc2ygj44zg7wgyypety42mps7rm0ry8n036upzg7yn3s203m2r";
  // const refUtxos = await api.utxosAt(addressHasRefScripts);
  // const refMintScript = refUtxos.find(x => 
  //   x.txHash == "cd739a53f330280c41f974a9c078f347ec59bd9c3362eb084f49d77e7d7b5316"
  // );
  // console.log('refMintScript: ', refMintScript);

  const tx = await api.newTx()
  // .readFrom([refMintScript])
  .collectFrom([operatorUtxo])
  .mintAssets({ [unit]: 1n }, redeemer)
  .attachMintingPolicy(mintingPolicy)
  .attachMetadata(label, metadataToken)
  .payToContract(managerContractAddress, { inline: datum }, { [unit]: 1n })
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
