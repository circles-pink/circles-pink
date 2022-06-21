import { pipe } from "fp-ts/lib/function";
import * as R from "fp-ts/lib/Record";
import * as A from "fp-ts/lib/Array";
import { join } from "path";
import { execSync } from "child_process";
import { readFileSync } from "fs";
import * as S from "fp-ts/lib/string";
import glob from "glob";

import ts, { StringLiteral } from "typescript";

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type WorkspaceName = string;

type WorkspaceInfo = { location: string; workspaceDependencies: string[] };

type WorkspacesInfo = Record<WorkspaceName, WorkspaceInfo>;

type PackageJson = { name: string; exports?: Record<string, string> };

type ImportLookup = Record<string, string>;

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const sourceGlob = "src/**/*.@(tsx|ts)";

// -----------------------------------------------------------------------------
// Impl
// -----------------------------------------------------------------------------

const parseImports = (code: string) => {
  const node = ts.createSourceFile("file.ts", code, ts.ScriptTarget.Latest);

  const importDecls: string[] = [];
  node.forEachChild((child) => {
    if (ts.SyntaxKind[child.kind] === "ImportDeclaration") {
      const importDecl = child as ts.ImportDeclaration;

      importDecls.push((importDecl.moduleSpecifier as StringLiteral).text);
    }
  });

  return importDecls;
};

const getPossiblePackages = (s: string): string[] => {
  const [first, second] = s.split("/");
  if (!first) return [];
  if (!first) return [first];
  return [first, `${first}/${second}`];
};

const isLocalImport =
  (wsi: WorkspacesInfo) =>
  (s: string): boolean => {
    if (s.startsWith("..")) return false;
    if (s.startsWith(".")) return false;

    const possibleNames = getPossiblePackages(s);

    return !!wsi[possibleNames[0]] || !!wsi[possibleNames[1]];
  };

const checkFile =
  (wsi: WorkspacesInfo, importLookup: ImportLookup) =>
  (path: string): number => {
    const code = readFileSync(path).toString();
    const imports = parseImports(code);

    const illegalImports = imports
      .filter(isLocalImport(wsi))
      .filter((i) => !importLookup[i])
      .map((i) => {
        console.log(`${path} -> "${i}"`);
        return i;
      });

    return illegalImports.length;
  };

const getSourceFiles = (ws: WorkspaceInfo): string[] => {
  const sourceFiles = glob.sync(join(ws.location, sourceGlob));

  return sourceFiles;
};

const resolveExports = (wsi: WorkspaceInfo): ImportLookup => {
  const packageJsonPath = join(wsi.location, "package.json");

  const packageJson: PackageJson = pipe(
    readFileSync(packageJsonPath).toString(),
    (i) => JSON.parse(i)
  );

  const resolvedExports = pipe(
    packageJson.exports || {},
    R.toEntries,
    A.map(
      ([k, v]) => [k.replace(/^./, packageJson.name), v] as [string, string]
    ),
    R.fromEntries
  );

  return resolvedExports;
};

const getImportLookup = (wsi: WorkspacesInfo): ImportLookup =>
  pipe(
    wsi,
    R.reduce(S.Ord)({}, (acc, x) => ({ ...acc, ...resolveExports(x) }))
  );

const getWorkspacesInfo = (): WorkspacesInfo =>
  pipe(execSync("yarn --silent workspaces info").toString(), (i) =>
    JSON.parse(i)
  );

const main = (): void => {
  const workspacesInfo: WorkspacesInfo = getWorkspacesInfo();

  const importLookup = getImportLookup(workspacesInfo);

  const failures = pipe(
    workspacesInfo,
    R.map((wsi) => {
      const sourceFiles = getSourceFiles(wsi);
      return pipe(
        sourceFiles,
        A.reduce(
          0,
          (acc, x) => acc + checkFile(workspacesInfo, importLookup)(x)
        )
      );
    }),
    R.reduce(S.Ord)(0, (acc, x) => acc + x)
  );

  console.log("");
  console.log(`${failures} illegal imports.`);

  if (failures !== 0) process.exit(1);
};

main();
