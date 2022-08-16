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

// const creatorSafeAddress = "0x5EA7424E7a512d6caAD95a8B1335c4159Fb29791";

// Find the nonce
const nonce = generateDeterministicNonceFromName("ExpeditionGrundeinkommen");
console.log("nonce:", nonce);

// Get a fresh safe Address
// sampleCore.safe.prepareDeploy(sampleAccount,{nonce: 95134672}).then(console.log)
const safeAddress = "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D";

// Register organization user
// sampleCore.user.register(sampleAccount, { nonce: 95134672, safeAddress: "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D", username: "ExpeditionGrundeinkommen", email: "info@expedition-grundeinkommen.de", avatarUrl: "https://directus.expedition-grundeinkommen.de/assets/ca35d273-d6b1-490b-b80e-12449bce5ef1" }).then(console.log)
// returns true

// Deploy safe for Organization
// sampleCore.safe.deployForOrganization(sampleAccount, { safeAddress: "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D" }).then(console.log)
// returns true

// Deploy Organization to the HUB
// sampleCore.organization.deploy(sampleAccount, { safeAddress: "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D" }).then(console.log)
// returns tx hash, e.g.: 0xea73fefd3ecd7a501db7842d662574edc4f3d216de2242f809f0218f9d763474

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
// sampleCore.organization.prefund(sampleAccount, { from: "0x5EA7424E7a512d6caAD95a8B1335c4159Fb29791", to: "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D", value: new BN("377125662824620100") }).then(console.log)
// returns tx hash, e.g.: 0xe6ecd6bc02111abaa68c78ae675d809223b473c581a20c69ede5bc9b96ef8204

// Trust new users
// sampleCore.trust.addConnection(sampleAccount, { user: "0xCCdfA2fa15C9d0Ba7e84A96341a54296873ABBa4", canSendTo: "0xB9AE1Ce83a6548f1395ddfC36673957B98Eb234D", limitPercentage: 100 }).then(console.log)
// returns tx hash, e.g.: 0xc982dda4a3d3d6bf9ef33eb7667118e5bed9f3c82a170348a0776cb67c921ea4
