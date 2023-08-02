import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import contract from "../../../built-contracts/credit-scoring.json" assert { type: "json" };

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

  // Payment private key
  const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));
  
  let wallet = await api.selectWalletFromPrivateKey(paymentKey.to_bech32());
  let address = await wallet.wallet.address();

  const plutusScript = {
    type: 'PlutusV2',
    script: contract.cborHex
  };

  const tx = await api.newTx()
  .payToAddressWithData(address, {
    scriptRef: plutusScript
  }, {})
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
