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
	rm -rf generated;

spago-build:
	spago build
	mkdir -p generated
	cp -r output -t generated

purs-tsd-gen:
	purs-tsd-gen --directory generated/output

patchTsTypes:
	patchTsTypes generated/output

generate: spago-build purs-tsd-gen patchTsTypes

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result