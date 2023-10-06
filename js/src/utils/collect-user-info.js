import * as lucid from "lucid-cardano";
import * as Blockfrost from '@blockfrost/blockfrost-js';

const url = 'https://cardano-preprod.blockfrost.io/api/v0';
const key = 'preprodqAq6ClZlZrpqNJpkdUli9KFNQE3WrtMZ';

const API = new Blockfrost.BlockFrostAPI({
  projectId: key,
  apiUrl: url
});

export async function collectUserInfo(userAddress) {
  //-------------------------------------------------------------------------

  // Collect user's data
  let addressBalance = 0n;
  let stakingReward = 0n;
  let numberTxs = 0n;
  let totalSent = 0n;

  // Get current month
  const currentDate = new Date();
  const currentMonth = currentDate.getMonth();
  const currentYear = currentDate.getFullYear();

  // Get previous month
  const monthToCollectData = currentMonth == 0 ? 11 : currentMonth - 1;
  const yearToCollectData = currentMonth == 0 ? currentYear - 1 : currentYear;
 
  // Get address balance
  console.log("Processing, please wait ...");
  const addressInfo = await API.addresses(userAddress);
  addressBalance = BigInt(Math.floor(addressInfo.amount[0].quantity / 1e6))
  console.log('addressBalance: ', addressBalance);

  // Get staking reward
  console.log("Processing, please wait ...");
  const rewardInfo = await API.accountsRewardsAll(addressInfo.stake_address)
  for (let i = rewardInfo.length - 1; i >= 0; i--) {
    const epochInfo = await API.epochs(rewardInfo[i].epoch);
    const date = new Date(epochInfo.end_time * 1e3);
    const monthToCheck = date.getMonth();
    const yearToCheck = date.getFullYear();
    if (monthToCollectData == monthToCheck && yearToCollectData == yearToCheck) {
      stakingReward += BigInt(rewardInfo[i].amount);
    } else if ((yearToCollectData > yearToCheck) || (yearToCollectData == yearToCheck && monthToCollectData > monthToCheck)) {
      break;
    }
  }
  stakingReward = stakingReward / BigInt(1e6);
  console.log('stakingReward: ', stakingReward);

  // Get number txs and total sent
  console.log("Processing, please wait ...");
  const txsInfo = await API.addressesTransactionsAll(userAddress);
  for (let i = txsInfo.length - 1; i >= 0; i--) {
    const date = new Date(txsInfo[i].block_time * 1e3);
    const monthToCheck = date.getMonth();
    const yearToCheck = date.getFullYear();
    if (monthToCollectData == monthToCheck && yearToCollectData == yearToCheck) {
      numberTxs += BigInt(1);

      const info = await API.txsUtxos(txsInfo[i].tx_hash);

      let sent = 0n
      let received = 0n;
      for (let item of info.inputs) {
        if (item.address == userAddress) {
          for (let value of item.amount) {
            if (value.unit == "lovelace") {
              sent += BigInt(value.quantity);
            }
          }
        }
      }
      for (let item of info.outputs) {
        if (item.address == userAddress) {
          for (let value of item.amount) {
            if (value.unit == "lovelace") {
              received += BigInt(value.quantity);
            }
          }
        }
      }
      let diff = sent - received;
      if (diff > 0n) {
        totalSent += diff;
      }
    } else if ((yearToCollectData > yearToCheck) || (yearToCollectData == yearToCheck && monthToCollectData > monthToCheck)) {
      break;
    }
  }
  totalSent = totalSent / BigInt(1e6);
  console.log('numberTxs: ', numberTxs);
  console.log('totalSent: ', totalSent);
  
  //-------------------------------------------------------------------------

  console.log(`Data: ${addressBalance}, ${stakingReward}, ${numberTxs}, ${totalSent}`);

  return {
    addressBalance,
    stakingReward,
    numberTxs,
    totalSent
  }
};
