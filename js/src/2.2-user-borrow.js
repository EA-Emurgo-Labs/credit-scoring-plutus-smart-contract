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

  // Lending package number that user want to borrow:
  //  + packageNumber = 1: 1000 ADA (score 1000 -> 2000 points)
  //  + packageNumber = 2: 2000 ADA (score 2001 -> 3000 points)
  //  + packageNumber = 3: 3000 ADA (score 3001 -> 4000 points)
  const packageNumber = 1;

  //-------------------------------------------------------------------------

  let amountToBorrow = 0;
  if (packageNumber == 1) {
    amountToBorrow = 1000;
  } else if (packageNumber == 2) {
    amountToBorrow = 2000;
  } else if (packageNumber == 3) {
    amountToBorrow = 3000;
  }

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
  // console.log('utxos: ', utxos);

  const nftUtxo = utxos.find(x =>
    x.assets[scoringNFT] == 1n
  )
  console.log('nftUtxo: ', nftUtxo);

  const contractUtxos = await api.utxosAt(LendingContractAddress);
  // console.log('contractUtxos: ', contractUtxos);

  const lendingUtxo = contractUtxos.find(x =>
    x.assets.lovelace == BigInt(amountToBorrow * 1e6)
  );
  console.log('lendingUtxo: ', lendingUtxo);

  const txInfo = await API.txsUtxos(nftUtxo.txHash);

  const previousDatumHash = txInfo.outputs[0].data_hash;
  const previousDatum = await API.scriptsDatum(previousDatumHash);
  console.log('previousDatum: ', JSON.stringify(previousDatum, 0, 4));

  const score = previousDatum.json_value.fields[0]["int"];
  // const score = 3000n;

  const owner = previousDatum.json_value.fields[1]["bytes"];
  // const owner = "";

  const lendingPackage = packageNumber;

  const redeemer = lucid.Data.to(
    new lucid.Constr(0, [])
  );

  const datum = lucid.Data.to(
    new lucid.Constr(0, [BigInt(score), owner, BigInt(lendingPackage)])
  );
  // console.log('datum: ', datum);

  console.log('test: ', BigInt(amountToBorrow * 1e6));

  const tx = await api.newTx()
  .collectFrom([nftUtxo, lendingUtxo], redeemer)
  .attachSpendingValidator(LendingContractScript)
  .payToAddress(userAddress, { lovelace: BigInt(amountToBorrow * 1e6) })
  .payToContract(LendingContractAddress, { inline: datum }, { [scoringNFT]: 1n })
  // .payToContract(LendingContractAddress, { inline: datum }, { lovelace: 1000000n })
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
