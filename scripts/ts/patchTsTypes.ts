import { join } from "path"
import * as fs from "fs"

const args = process.argv.slice(2)

const DIR = args[0];

const filePath = join(DIR, "Effect.Promise/index.d.ts");

const oldSrc = fs.readFileSync(filePath).toString();

const newSrc = oldSrc.replace(/\$\$Promise<a0> = any/, "Promise<a0> = Promise<a0>")

fs.writeFileSync(filePath, newSrc)