import { join } from "path"
import * as fs from "fs"

const args = process.argv.slice(2)

const DIR = args[0];

{
    const filePath = join(DIR, "Effect.Promise/index.d.ts");

    const oldSrc = fs.readFileSync(filePath).toString();

    const newSrc = oldSrc
        .replace(/\$\$Promise<a0> = any/, "Promise<a0> = Promise<a0>")

    fs.writeFileSync(filePath, newSrc)
}

{
    const filePath = join(DIR, "Effect.Aff/index.d.ts");

    const oldSrc = fs.readFileSync(filePath).toString();

    const newSrc = oldSrc
        .replace(/ Aff<a0> = any/, " Aff<a0> = { readonly _Aff : unique symbol; }")

    fs.writeFileSync(filePath, newSrc)
}

