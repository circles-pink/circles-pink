dev-storybook:
	npm run storybook --prefix pkgs/ts

build-storybook:
	export OUTPUT_DIR=../../dist; npm run build --prefix pkgs/ts

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;

npm-install:
	npm install --prefix pkgs/ts

clean-install:
	rm -rf node_modules
	rm -rf pkgs/ts/*/node_modules

clean-materialized:
	rm -rf materialized

materialize: clean-materialized spago2nix

clean-generate:
	rm -rf generated;

spago-build:
	spago build
	mkdir -p generated
	cp -r output -t generated

purs-tsd-gen:
	purs-tsd-gen --directory generated/output

patchTsTypes:
	patchTsTypes $(PWD)/generated/output

generate: materialize spago-build purs-tsd-gen patchTsTypes

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

ci: generate
	nix build .#ts.builds.storybook
