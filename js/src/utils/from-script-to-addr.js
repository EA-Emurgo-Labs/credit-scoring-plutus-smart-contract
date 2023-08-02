import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import marketplaceContract from "../../../built-contracts/credit-scoring.json" assert { type: "json" };

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

  const marketplaceContractScript = {
    type: "PlutusV2",
    script: marketplaceContract.cborHex,
  };

  const marketplaceContractAddress = api.utils.validatorToAddress(
    marketplaceContractScript
  );
  // console.log('marketplaceContractAddress: ', marketplaceContractAddress);

  // const marketplaceContractPKH = lucid.getAddressDetails("addr_test1vzym9zs9h9w6yexdxys5kvehn7jw2yee3yy64t65vfg4hqswdtwh9");
  const marketplaceContractPKH = lucid.getAddressDetails(marketplaceContractAddress).paymentCredential?.hash || "";
  console.log('marketplaceContractPKH: ', marketplaceContractPKH);

})().catch(error => {
  console.log('error: ', error);
});
