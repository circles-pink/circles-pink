const fs = require("fs");
const lockfile = require("@yarnpkg/lockfile");

const args = process.argv.slice(2);

const pathLockFile = args[0] || "yarn.lock";

let file = fs.readFileSync(pathLockFile, "utf8");
let json = lockfile.parse(file);

if (!json.type === "success") throw new Error();

console.log(JSON.stringify(json.object, null, 2));
