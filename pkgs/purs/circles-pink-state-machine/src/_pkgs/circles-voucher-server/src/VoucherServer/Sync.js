const { crcToTc } = require("@circles/timecircles");

var web3 = require("web3");
const Web3 = new web3();

exports.frecklesToEuroCentImpl = (timestamp) => (bn) => {
    const freckles = bn.toString();
    const crc = Number.parseFloat(Web3.utils.fromWei(freckles, "ether"));
    const tc = crcToTc(timestamp, crc);
    const eur = Math.round(tc * 10);
    return eur;
  };
  