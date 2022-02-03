dev-storybook:
	npm run storybook --prefix pkgs/ts

build-storybook:
	export OUTPUT_DIR=../../dist; npm run build --prefix pkgs/ts

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;

node2nix:
	mkdir -p materialized/node2nix;
	node2nix \
	  --input pkgs/ts/package.json \
	  --lock pkgs/ts/package-lock.json \
	  --composition materialized/node2nix/default.nix \
	  --node-env materialized/node2nix/node-env.nix \
	  --output materialized/node2nix/node-packages.nix

npm-install:
	npm install --prefix pkgs/ts

clean-materialized:
	rm -rf materialized

materialize: clean-materialized spago2nix node2nix

clean-generate:
	rm -rf pkgs/ts/src/generated;

spago-build:
	spago build
	mkdir -p pkgs/ts/src/generated
	cp -r output -t pkgs/ts/src/generated

purs-tsd-gen:
	purs-tsd-gen --directory pkgs/ts/src/generated/output

patchTsTypes:
	patchTsTypes pkgs/ts/src/generated/output

generate: spago-build purs-tsd-gen patchTsTypes

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

ci:
	nix build .#ts.default
