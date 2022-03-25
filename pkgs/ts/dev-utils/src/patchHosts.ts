import { string } from "fp-ts";
import { pipe } from "fp-ts/function";

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type IP = string;
type Path = string;
type Domain = string;

type Config = {
  hostsfile: Path;
  services: Record<IP, Domain>;
};

type Opts = [Path];

type Caps = {
  getArgv: () => Opts;
  readFile: (path: Path) => string;
  writeFile: (path: Path, content: string) => void;
};

// -----------------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------------

const test = () => {
  const vfs: Record<string, string> = {};

  const caps: Caps = {
    getArgv: () => ["config.json"],
    readFile: (path) => {
      switch (path) {
        case "config.json":
          return "";
        case "/etc/hosts":
          return "";
        default:
          throw new Error();
      }
    },
    writeFile: (p, c) => {
      vfs[p] = c;
    },
  };

  run(caps);
};

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------

const run = (cap: Caps) => {
  const opts = cap.getArgv();
  const config = pipe(cap.readFile(opts[0]), JSON.parse) as Config;
  console.log(config);
};

const main = () => {
  const caps: Caps = {
    getArgv: () => process.argv.slice(2) as Opts,
    readFile: () => "",
    writeFile: (p, c) => {},
  };

  run(caps);
};

main();
