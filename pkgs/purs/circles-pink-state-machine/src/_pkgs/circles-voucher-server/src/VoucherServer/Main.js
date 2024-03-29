const crypto = require("crypto");
const { crcToTc, tcToCrc } = require("@circles/timecircles");
var web3 = require("web3");
const Web3 = new web3();

exports.decryptImpl = (Nothing) => (Just) => (secretKey) => (data) => {
  try {
    const decipheriv = crypto.createDecipheriv("AES-256-ECB", secretKey, null);
    let decryptediv = decipheriv.update(data, "base64", "utf8");
    decryptediv += decipheriv.final("utf8");
    return Just(decryptediv);
  } catch (e) {
    return Nothing;
  }
};

exports.encryptImpl = (Nothing) => (Just) => (secretKey) => (data) => {
  try {
    const cipher = crypto.createCipheriv("AES-256-ECB", secretKey, null);
    let encrypted = cipher.update(data, "utf8", "base64");
    encrypted += cipher.final("base64");
    return Just(encrypted);
  } catch (e) {
    return Nothing;
  }
};
