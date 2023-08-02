import * as Blockfrost from '@blockfrost/blockfrost-js';

const API = new Blockfrost.BlockFrostAPI({
  projectId: 'preproducgQyCbIAtRpfRQGcs7DqBOjN97VPQtd',
  apiUrl: 'https://cardano-preprod.blockfrost.io/api/v0'
});

(async function test() {
  try {
    // const networkInfo = await API.network();
    // const latestEpoch = await API.epochsLatest();
    // const health = await API.health();

    // console.log('networkInfo', networkInfo);
    // console.log('latestEpoch', latestEpoch);
    // console.log('health', health);

    const data = await API.scriptsDatum('6fd25c9cd5cfe8d09fb2d88809db1abfff57d2807a8643cc843939bf316bc7b5');
    console.log('data: ', JSON.stringify(data, 0, 4))

    // const data = await API.assetsById('1f433e0405a5770b39bda838ffbaff745520fe99a91e7e19c83230d94d6f6f6e7374616b654576656e7432303233');
    // console.log('data: ', data);
    // Output:
    // data:  {
    //   asset: '1f433e0405a5770b39bda838ffbaff745520fe99a91e7e19c83230d94d6f6f6e7374616b654576656e7432303233',
    //   policy_id: '1f433e0405a5770b39bda838ffbaff745520fe99a91e7e19c83230d9',
    //   asset_name: '4d6f6f6e7374616b654576656e7432303233',
    //   fingerprint: 'asset1fy3j6rw7jwl54g46nntpmrrep33jqhhx0ep352',
    //   quantity: '6',
    //   initial_mint_tx_hash: '2d1937c901a029e5d2d930274e73b52965dc36da666cf89c67d8c97703480d8f',
    //   mint_or_burn_count: 6,
    //   onchain_metadata: {
    //     id: '1',
    //     name: 'Moonstake Event 2023',
    //     image: 'ipfs://QmZKhZQr9RDMtZqEbkXCSPWCyKxrs9S5bFTNjaB4TPHHQw',
    //     description: "This is Moonstake's NFT"
    //   },
    //   onchain_metadata_standard: 'CIP25v1',
    //   metadata: null
    // }

    // const txinfo = await API.txsUtxos('7d0823909cd28072523113132d12ca60b15f9a820a14cd573489c0d4617ff211');
    // console.log('txinfo: ', txinfo.outputs[0].amount);
    
  } catch (err) {
    console.log('error', err);
  }
})();
