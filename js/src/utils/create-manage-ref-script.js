import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import contract from "../../../built-contracts/manage-score.json" assert { type: "json" };

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

  // Address to receive the reference script
  const toAddress = "addr_test1qzqcdfglhu5dj5kr5lzndv8523m9rw52sjnyqrrdskdss884fc2ygj44zg7wgyypety42mps7rm0ry8n036upzg7yn3s203m2r"

  //-------------------------------------------------------------------------

  const wallet = await api.selectWalletFromSeed(
    mnemonic,
    {
      addressType: "Base",
      accountIndex: accountIndex
    }
  );

  const plutusScript = {
    type: 'PlutusV2',
    script: contract.cborHex
  };

  const tx = await api.newTx()
  .payToAddressWithData(toAddress, {
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
