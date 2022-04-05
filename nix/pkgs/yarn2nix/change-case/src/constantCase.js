const { constantCase } = require("change-case");

const args = process.argv.slice(2);

console.log(constantCase(args[0]));
