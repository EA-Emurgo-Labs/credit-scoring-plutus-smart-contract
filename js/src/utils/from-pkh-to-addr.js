import * as lucid from "lucid-cardano";

(function convert() {
  const privateKey = lucid.C.PrivateKey.from_normal_bytes(Buffer.from("cd2f0c43542705d8318a4ea48e5e457ef7f2a4f012a79d8ea73e83d56f0ab642", "hex"));
  
  const pubkeyHash = privateKey.to_public().hash().to_hex();
  console.log('pubkeyHash: ', pubkeyHash);
  
  const paymentCredential = lucid.C.StakeCredential.from_keyhash(lucid.C.Ed25519KeyHash.from_hex(pubkeyHash));
  const address = lucid.C.EnterpriseAddress.new(0, paymentCredential).to_address().to_bech32();
  console.log('address: ', address);
  
  const convertAddressToPKH = lucid.getAddressDetails(address).paymentCredential?.hash || "";
  console.log('convertAddressToPKH: ', convertAddressToPKH);
})();
