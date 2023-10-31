import * as lucid from "lucid-cardano";

(function convert() {
  const address = "addr_test1qq55wtma8puzyd7qgayvup3hkwa0uykpryhlq4ztvx7u5rsa7vtthm0ckftags3482680kep7hkt8c5q2mg8nmc9mk6s3x9de0";
  
  const convertAddressToPKH = lucid.getAddressDetails(address).paymentCredential?.hash || "";
  console.log('convertAddressToPKH: ', convertAddressToPKH);

  const convertAddressToSH = lucid.getAddressDetails(address).stakeCredential?.hash || "";
  console.log('convertAddressToSH: ', convertAddressToSH);

  let spendCred = lucid.C.StakeCredential.from_keyhash(lucid.C.Ed25519KeyHash.from_hex(convertAddressToPKH));
  let stakeCred = lucid.C.StakeCredential.from_keyhash(lucid.C.Ed25519KeyHash.from_hex(convertAddressToSH));
  let convertToAddress = lucid.C.BaseAddress.new(0, spendCred, stakeCred).to_address().to_bech32();
  console.log('convertToAddress: ', convertToAddress);
})();
