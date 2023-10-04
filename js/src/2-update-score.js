import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
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

  // Address to update score
  const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

  //-------------------------------------------------------------------------

  // Collect user's data

  const addressBalance = 1000;
  const stakingReward = 1;
  const numberTxs = 5;
  const totalSent = 200;

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

  // Constants

  const MINUS_POINTS_IF_LATE_PAYMENT = 10;

  //-------------------------------------------------------------------------

  const newPointsOfFactors = [pointsOfFactor0, pointsOfFactor1, pointsOfFactor2, pointsOfFactor3];

  const weights = [weightOfFactor0, weightOfFactor1, weightOfFactor2, weightOfFactor3];

  //-------------------------------------------------------------------------

  const userPKH = lucid.getAddressDetails(userAddress).paymentCredential?.hash || "";
  const userSH = lucid.getAddressDetails(userAddress).stakeCredential?.hash || "";

  // Redeemer
  const redeemer = lucid.Data.to(
    new lucid.Constr(0, [newPointsOfFactors, weights])
  );  

  // Calculate the new base score 
  let newBaseScore = 0n;
  for (let i = 0; i < newPointsOfFactors.length; i++) {
    newBaseScore += newPointsOfFactors[i] * weights[i];
  }
  console.log(`newBaseScore: ${newBaseScore}`);

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

  const manageContractScript = {
    type: "PlutusV2",
    script: manageContract.cborHex,
  };
  const manageContractAddress = api.utils.validatorToAddress(
    manageContractScript
  );
  console.log('manageContractAddress: ', manageContractAddress);

  const contractUtxos = await api.utxosAt(manageContractAddress);
  console.log('contractUtxos: ', contractUtxos);

  let mainUtxo = null;
  let txid = null;
  let unit = null;
  let ownerPKH = null;
  let ownerSH = null;
  let lendingScore = null;
  let lendingPackage = null;
  let deadlinePayback = null;
  for (let item of contractUtxos) {
    let txInfo = await API.txsUtxos(item.txHash);

    unit = txInfo.outputs[0].amount[1].unit;
    console.log('unit: ', unit);

    let previousDatumHash = txInfo.outputs[0].data_hash;
    let previousDatum = await API.scriptsDatum(previousDatumHash);

    // New datum
    ownerPKH = previousDatum.json_value.fields[0]["bytes"];
    ownerSH = previousDatum.json_value.fields[1]["bytes"];

    if (ownerPKH == userPKH && ownerSH == userSH) {
      lendingScore = BigInt(previousDatum.json_value.fields[3]["int"]);
      lendingPackage = BigInt(previousDatum.json_value.fields[4]["int"]);
      deadlinePayback = BigInt(previousDatum.json_value.fields[5]["int"]);
      mainUtxo = item;
      break;
    }
  }
  if (mainUtxo == null) {
    throw new Error("Cannot find the main utxo to update user's score");
  }
  console.log("Main utxo: ", mainUtxo);

  const unixTimeStamp = Math.floor(Date.now());
  console.log("unixTimeStamp: ", unixTimeStamp);

  if (unixTimeStamp > deadlinePayback) {
    lendingScore = lendingScore - BigInt(MINUS_POINTS_IF_LATE_PAYMENT);
  }

  console.log("New datum: ", ownerPKH, ownerSH, newBaseScore, lendingScore, lendingPackage, deadlinePayback);
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [ownerPKH, ownerSH, newBaseScore, lendingScore, lendingPackage, deadlinePayback])
  );

  const addressHasRefScripts = "addr_test1qzqcdfglhu5dj5kr5lzndv8523m9rw52sjnyqrrdskdss884fc2ygj44zg7wgyypety42mps7rm0ry8n036upzg7yn3s203m2r";
  const refUtxos = await api.utxosAt(addressHasRefScripts);
  const refManageScript = refUtxos.find(x => 
    x.txHash == "a2e6fd9df4a9e85f163d286f755a0d471fab8941b3605e5a340f6a77a2181216"
  );
  console.log('refManageScript: ', refManageScript);

  const currentSlot = await api.currentSlot();
  console.log('currentSlot: ', currentSlot);

  const validFrom = await lucid.slotToBeginUnixTime(currentSlot - 100, lucid.SLOT_CONFIG_NETWORK.Preprod);
  console.log('validFrom: ', validFrom);

  const validTo = await lucid.slotToBeginUnixTime(currentSlot + 100, lucid.SLOT_CONFIG_NETWORK.Preprod);
  console.log('validTo: ', validTo);

  const tx = await api.newTx()
  .readFrom([refManageScript])
  .collectFrom([operatorUtxo, mainUtxo], redeemer)
  .payToContract(manageContractAddress, { inline: datum }, { [unit]: 1n })
  .validFrom(validFrom)
  .validTo(validTo)
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
