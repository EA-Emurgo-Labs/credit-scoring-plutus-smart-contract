import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';

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

  const paymentKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));

  let wallet = await api.selectWalletFromPrivateKey(paymentKey.to_bech32());
  let address = await wallet.wallet.address();
  console.log('address: ', address);

  const userAddress = "addr_test1vrlqualxp9m5juf7knj07mr9d32nwmmr85vkmkxgulrzxfqq3w6g5";

  const tx = await api.newTx()
  .payToAddress(userAddress, { lovelace: 1000000n })
  .complete();

  const signedTx = await tx.sign().complete();

  const txHash = await signedTx.submit();

  console.log(txHash);
})();
