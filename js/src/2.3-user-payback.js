import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import LendingContract from "../../built-contracts/lending.json" assert { type: "json" };

const url = "https://cardano-preprod.blockfrost.io/api/v0";
const key = "preproducgQyCbIAtRpfRQGcs7DqBOjN97VPQtd";

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

  // Private key of user address
  const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));

  // Scoring NFT
  const scoringNFT = "29945394e952ab8b98d0619a7bb1ec186045cfd1155a1dee50eed0ef53636f72696e674e4654456d7572676f4c616273";

  //-------------------------------------------------------------------------

  const LendingContractScript = {
    type: "PlutusV2",
    script: LendingContract.cborHex,
  };

  const LendingContractAddress = api.utils.validatorToAddress(
    LendingContractScript
  );
  console.log('LendingContractAddress: ', LendingContractAddress);

  const wallet = await api.selectWalletFromPrivateKey(paymentKey.to_bech32());
  const userAddress = await wallet.wallet.address();

  const utxos = await api.utxosAt(userAddress);
  console.log('utxos: ', utxos);

  const contractUtxos = await api.utxosAt(LendingContractAddress);
  console.log('contractUtxos: ', contractUtxos);

  let nftUtxo = null;
  let score = 0;
  let owner = "";
  let packageNumber = 0;

  for (const utxo of contractUtxos) {
    const txInfo = await API.txsUtxos(utxo.txHash);
    // console.log('txInfo: ', txInfo);

    let contractOutput = null;
    for (const output of txInfo.outputs) {
      if (output.address == LendingContractAddress) {
        contractOutput = output;
      }
    }
    console.log('contractOutput: ', contractOutput);

    const previousDatumHash = contractOutput.data_hash;

    const previousDatum = await API.scriptsDatum(previousDatumHash);
    console.log('previousDatum: ', JSON.stringify(previousDatum, 0, 4));

    const ownerInDatum = previousDatum.json_value.fields[1]["bytes"];

    if (ownerInDatum == lucid.getAddressDetails(userAddress).paymentCredential?.hash) {
      nftUtxo = utxo;
      score = previousDatum.json_value.fields[0]["int"];
      // const score = 3000n;
      owner = previousDatum.json_value.fields[1]["bytes"];
      // const owner = "";
      packageNumber = previousDatum.json_value.fields[2]["int"];
    }
  }

  console.log('nftUtxo: ', nftUtxo);
  console.log('score: ', score);
  console.log('owner: ', owner);
  console.log('packageNumber: ', packageNumber);

  let amountToPayback = 0;
  if (packageNumber == 1) {
    amountToPayback = 1000;
  } else if (packageNumber == 2) {
    amountToPayback = 2000;
  } else if (packageNumber == 3) {
    amountToPayback = 3000;
  }
  console.log('amountToPayback: ', amountToPayback);

  const redeemer = lucid.Data.to(
    new lucid.Constr(1, [])
  );

  const datumNFT = lucid.Data.to(
    new lucid.Constr(0, [BigInt(score), owner, 0n])
  );

  const datumLendingPackage = lucid.Data.to(
    new lucid.Constr(0, [BigInt(packageNumber)])
  );

  // console.log('test: ', BigInt(amountToBorrow * 1e6));

  const tx = await api.newTx()
  .collectFrom([nftUtxo], redeemer)
  .attachSpendingValidator(LendingContractScript)
  .payToAddressWithData(userAddress, { inline: datumNFT }, { [scoringNFT]: 1n })
  // .payToAddressWithData(userAddress, { inline: lucid.Data.void() }, { lovelace: 1000000n })
  .payToContract(LendingContractAddress, { inline: datumLendingPackage }, { lovelace: BigInt(amountToPayback * 1e6) })
  // .payToContract(LendingContractAddress, { inline: lucid.Data.void() }, { lovelace: 1000000n })
  .addSigner(userAddress)
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
