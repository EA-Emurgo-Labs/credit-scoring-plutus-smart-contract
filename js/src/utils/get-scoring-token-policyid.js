import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';
import contract from "../../../built-contracts/mint-score.json" assert { type: "json" };

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

  const mintingPolicy = {
    type: 'PlutusV2',
    script: lucid.applyParamsToScript(
      contract.cborHex,
      []
    )
  };

  const policyId = api.utils.mintingPolicyToId(
    mintingPolicy
  );

  console.log('policyId: ', policyId);
})().catch(error => {
  console.log('error: ', error);
});
