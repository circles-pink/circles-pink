const web3 = require("web3");
const { crcToTc, tcToCrc } = require("@circles/timecircles");

const Web3 = new web3();

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const generateDeterministicNonceFromName = (str: string) => {
  // Convert each character in the string to its regarding character code
  const charCodeStr = str.split("").reduce((acc, char) => {
    return `${acc}${char.charCodeAt(0)}`;
  }, "");
  // Make this number smaller
  return parseInt(charCodeStr.slice(0, 30)) % 123456789;
};

// -----------------------------------------------------------------------------
// Documentation
// -----------------------------------------------------------------------------

// sampleCore and sampleAccount must be parametrized with creator priv key!

const creatorSafeAddress = "0x6ea9a9993b5372317a9679f8dd21668288484989";

// Find the nonce
const nonce = generateDeterministicNonceFromName("xbge");
console.log(nonce);

// Get a fresh safe Address
// sampleCore.safe.prepareDeploy(sampleAccount,{nonce: 122794568}).then(console.log)
const safeAddress = "0x315708c65B3A0e12604C287108638C6163581415";

// Register organization user
// sampleCore.user.register(sampleAccount, { nonce: 122794568, safeAddress: "0x315708c65B3A0e12604C287108638C6163581415", username: "xbge", email: "info@xbge.de", avatarUrl: "https://sample-image.de" }).then(console.log)
// returns true

// Deploy safe for Organization
// sampleCore.safe.deployForOrganization(sampleAccount, { safeAddress: "0x315708c65B3A0e12604C287108638C6163581415" }).then(console.log)
// returns true

// Deploy Organization to the HUB
// sampleCore.organization.deploy(sampleAccount, { safeAddress: "0x315708c65B3A0e12604C287108638C6163581415" }).then(console.log)
// returns tx hash, e.g.: 0x7df61d841507513191662075d5b18909754dfea9479ba0fb2958419ba4176dc2

const timeCircles = tcToCrc(Date.now(), Number(1));
console.log(timeCircles);

const circlesAmount = crcToTc(Date.now(), 1);
console.log("circlesAmount:", circlesAmount);

// core.utils.toFreckles(timeCircles)
// const freckles = "377124244451084170"; "377125662824620100"

const freckles = Number.parseFloat(
  Web3.utils.toWei("0.3771256628246201", "ether")
);
console.log("freckles", freckles);

// const amount = new web3.utils.BN(freckles);
// console.log(amount);

// Prefund Org account
// sampleCore.organization.prefund(sampleAccount, { from: "0x6eA9a9993B5372317a9679f8dD21668288484989", to: "0x315708c65B3A0e12604C287108638C6163581415", value: new BN("377124244451084170") }).then(console.log)
// returns tx hash, e.g.: 0x65b83027bf2b5303f25b74c715e53d44bc413496a51a27fd55796f4731d2e98d
