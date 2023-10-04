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

  const userAddress = "addr_test1qq0lw3vz5r4tagknlpmc2w07f7e3ccjfe59l2gld8phqym8t7fp2tn0gunjrlsvg4qgyrq7k2urz276hs6fzj8lcqf3qnek6vg";

  //-------------------------------------------------------------------------

  // Data Model

  //-------------------------------------------------------------------------

  const manageContractScript = {
    type: "PlutusV2",
    script: manageContract.cborHex,
  };
  const manageContractAddress = api.utils.validatorToAddress(
    manageContractScript
  );
  console.log('manageContractAddress: ', manageContractAddress);

  const newPointsOfFactors = [20n, 10n];

  const weights = [50n, 50n];

  // Redeemer
  const redeemer = lucid.Data.to(
    new lucid.Constr(0, [newPointsOfFactors, weights])
  );  

  // Datum 
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

  const contractUtxo = await api.utxosAt(manageContractAddress);
  console.log('contractUtxo: ', contractUtxo[0]);

  const utxos = await api.utxosAt(operatorAddress);
  console.log('utxos: ', utxos);

  let operatorUtxo = utxos.find(x => 
    x.assets[operatorId] == 1n
  );
  console.log('operatorUtxo: ', operatorUtxo);

  const txInfo = await API.txsUtxos("84e769c7f999c59575f62709f9fe17e84e299c69a9147d6495be2ca9120d4800");

  const unit = txInfo.outputs[0].amount[1].unit;
  console.log('unit: ', unit);

  const previousDatumHash = txInfo.outputs[0].data_hash;
  const previousDatum = await API.scriptsDatum(previousDatumHash);

  // New datum
  const ownerPKH = previousDatum.json_value.fields[0]["bytes"];
  const ownerSH = previousDatum.json_value.fields[1]["bytes"];
  const lendingScore = BigInt(previousDatum.json_value.fields[3]["int"]);
  const lendingPackage = BigInt(previousDatum.json_value.fields[4]["int"]);
  const deadlinePayback = BigInt(previousDatum.json_value.fields[5]["int"]);

  console.log("new datum: ", ownerPKH, ownerSH, newBaseScore, lendingScore, lendingPackage, deadlinePayback)
  
  const datum = lucid.Data.to(
    new lucid.Constr(0, [ownerPKH, ownerSH, newBaseScore, lendingScore, lendingPackage, deadlinePayback])
  );

  const tx = await api.newTx()
  // .collectFrom([...utxos, contractUtxo[0]], redeemer)
  .collectFrom([operatorUtxo, contractUtxo[0]], redeemer)
  .attachSpendingValidator(manageContractScript)
  // .payToContract(userAddress, { inline: datum }, { [unit]: 1n })
  // .payToContract(manageContractAddress, { inline: lucid.Data.void() }, { [unit]: 1n })
  .payToContract(manageContractAddress, { inline: datum }, { [unit]: 1n })
  .complete();

  const signedTx = await tx.sign().complete();

  // try {
  //   const txHash = await signedTx.submit();
  //   console.log('hash: ', txHash);
  // } catch (error) {
  //   console.log('error: ', error);
  // }
})().catch(error => {
  console.log('error: ', error);
});
