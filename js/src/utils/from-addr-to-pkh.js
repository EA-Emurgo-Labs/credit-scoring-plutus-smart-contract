import * as lucid from "lucid-cardano";

(function convert() {
  const address = "addr_test1qq0snluqgfj0gpcmthyavglnu6xyzsc6fr8x5ha93ca0jlpa84ruhgqg4usxw6gnuqfsevzwc5ed5suqqg83z78zvdksge9fnu";
  
  const convertAddressToPKH = lucid.getAddressDetails(address).paymentCredential?.hash || "";
  console.log('convertAddressToPKH: ', convertAddressToPKH);

  const convertAddressToSH = lucid.getAddressDetails(address).stakeCredential?.hash || "";
  console.log('convertAddressToSH: ', convertAddressToSH);

  let spendCred = lucid.C.StakeCredential.from_keyhash(lucid.C.Ed25519KeyHash.from_hex(convertAddressToPKH));
  let stakeCred = lucid.C.StakeCredential.from_keyhash(lucid.C.Ed25519KeyHash.from_hex(convertAddressToSH));
  let convertToAddress = lucid.C.BaseAddress.new(0, spendCred, stakeCred).to_address().to_bech32();
  console.log('convertToAddress: ', convertToAddress);
})();
