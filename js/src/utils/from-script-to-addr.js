// import * as lucid from "lucid-cardano";
// import * as Blockfrost from '@blockfrost/blockfrost-js';
// import contract from "../../../built-contracts/manage-score.json" assert { type: "json" };
// 
// const url = 'https://cardano-preprod.blockfrost.io/api/v0';
// const key = 'preprodqAq6ClZlZrpqNJpkdUli9KFNQE3WrtMZ';
// 
// const API = new Blockfrost.BlockFrostAPI({
//   projectId: key,
//   apiUrl: url
// });
// 
// (async function test() {
//   const api = await lucid.Lucid.new(
//     new lucid.Blockfrost(url, key),
//     "Preprod"
//   );
// 
//   const contractScript = {
//     type: "PlutusV2",
//     script: contract.cborHex,
//   };
// 
//   const contractAddress = api.utils.validatorToAddress(
//     contractScript
//   );
//   console.log('contractAddress: ', contractAddress);
// 
//   const contractHash = lucid.getAddressDetails(contractAddress).paymentCredential?.hash || "";
//   console.log('contractHash: ', contractHash);
// 
// })().catch(error => {
//   console.log('error: ', error);
// });
