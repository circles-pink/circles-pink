import { join } from "path";
import * as fs from "fs";
import * as glob from "glob";

const args = process.argv.slice(2);

const DIR = args[0];

{
  const filePath = join(DIR, "Effect.Aff/index.d.ts");

  const oldSrc = fs.readFileSync(filePath).toString();

  const newSrc = oldSrc.replace(
    / Aff<a0> = any/,
    " Aff<a0> = { readonly _Aff : unique symbol; }"
  );

  fs.writeFileSync(filePath, newSrc);
}

{
  const files = glob.sync(join(DIR, "*/*.d.ts"));

  for (let filePath of files) {
    const oldSrc = fs.readFileSync(filePath).toString();

    const newSrc = oldSrc.replace(
      /any/g,
      "{ readonly Opaque : unique symbol; }"
    );

    fs.writeFileSync(filePath, newSrc);
  }
}

{
  const files = glob.sync(join(DIR, "*/*.d.ts"));

  for (let filePath of files) {
    const oldSrc = fs.readFileSync(filePath).toString();

    const newSrc = oldSrc.replace(/: unsupported kind/g, "");

    fs.writeFileSync(filePath, newSrc);
  }
}

{
  const filePath = join(DIR, "Milkis.Impl.Window/foreign.js");

  const oldSrc = fs.readFileSync(filePath).toString();

  const newSrc = oldSrc.replace(
    /exports.windowFetch = window.fetch;/,
    'exports.windowFetch = typeof window !== "undefined" ? window.fetch : {};'
  );

  fs.writeFileSync(filePath, newSrc);
}
