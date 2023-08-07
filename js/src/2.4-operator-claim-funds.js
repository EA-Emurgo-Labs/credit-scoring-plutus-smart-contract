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

  // Private key of operator address
  const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cc6beca62a4436767a309bb61e7266434449b2a63f4f7a3b60bd5a726407f3f8", "hex"));

  // Operator token
  const operatorId = 'c7e02489157f1e9f56daed93de0a2c9b5ab8cabf700cd6e14f7a5f124d6f6f6e7374616b65546573746e657431';

  // Scoring NFT
  const scoringNFT = "d9a6c3ebf6492749f6063a1851d6607c1c8ce289da762c11a19bd32253636f72696e674e4654456d7572676f4c616273";

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
  const operatorAddress = await wallet.wallet.address();

  const utxos = await api.utxosAt(operatorAddress);
  console.log('utxos: ', utxos);

  let operatorUtxo = utxos.find(x => 
    x.assets[operatorId] == 1n
  );
  console.log('operatorUtxo: ', operatorUtxo);

  const contractUtxos = await api.utxosAt(LendingContractAddress);
  console.log('contractUtxos: ', contractUtxos);

  let amountToClaim = 0n;
  let claimUtxos = [];
  for (let utxo of contractUtxos) {
    // console.log("utxo.assets[scoringNFT]: ", utxo.assets[scoringNFT])
    if (utxo.assets[scoringNFT] == undefined) {
      amountToClaim += utxo.assets.lovelace;
      claimUtxos.push(utxo);
    }
  }

  console.log("claimUtxos: ", claimUtxos);
  console.log("amountToClaim: ", amountToClaim);

  const redeemer = lucid.Data.to(
    new lucid.Constr(2, [])
  );

  const tx = await api.newTx()
  // .collectFrom([...claimUtxos], redeemer)
  .collectFrom([operatorUtxo, ...claimUtxos], redeemer)
  .attachSpendingValidator(LendingContractScript)
  .payToAddress(operatorAddress, { lovelace: amountToClaim })
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
