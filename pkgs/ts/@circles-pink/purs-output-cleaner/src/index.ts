import { execSync } from "child_process";
import glob from "glob";
import { pipe } from "fp-ts/lib/function";
import * as A from "fp-ts/lib/Array";
import { readFileSync, rmSync } from "fs";
import yargs from 'yargs/yargs';
import { join } from "path/posix";
import { basename } from "path";


type Args = {output: string}

const getArgs = (): Args => yargs(process.argv.slice(2)).options({
  output: { type: 'string', default: "output" },
}).parseSync();

const getSpagoSources = (): string[] =>
  pipe(execSync("spago sources").toString(), (i) => i.trim().split("\n"));

const getFiles = (globs: string[]): string[] => {
  const accum: Array<string> = [];

  globs.forEach((glob_) => {
    const files = glob.sync(glob_);
    files.forEach((file) => {
      accum.push(file);
    });
  });

  return accum;
};

const getModule = (path: string): string => {
  const src = readFileSync(path).toString();

  const matches = src.match(/^module ([A-Za-z0-9.]*)/m);

  if (matches === null) throw new Error(`invalid module: ${path}`)

  const moduleName = matches[1]

  if (typeof moduleName === "undefined") throw new Error("invalid module")

  return moduleName;
};

const getModules = (files: string[]): string[] => pipe(files, A.map(getModule));

const removeOldDirs = (args: Args, modules: Set<string> ) => {
  const compiledDirs = glob.sync(join(args.output, "*/"))

  compiledDirs.forEach(compiledDir => {
    const moduleName = basename(compiledDir)
    if (!modules.has(moduleName)) {
      console.log(`Deleting old compiled module ${moduleName}`)
      rmSync(compiledDir, { recursive: true, force: true });
    }
  })

}

const main = () => {
  const args = getArgs()

  const globs = getSpagoSources();

  const files = getFiles(globs);

  const modules = new Set(getModules(files));

  removeOldDirs(args, modules)
};

main();
