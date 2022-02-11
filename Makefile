dev-storybook:
	yarn workspace storybook run storybook

build-storybook:
	export OUTPUT_DIR=../../../dist; yarn workspace storybook run build

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;

yarn-install:
	yarn install

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
	purs-tsd-gen --directory generated/output && \
	cp -r generated/output pkgs/ts/generated

patchTsTypes:
	patchTsTypes $(PWD)/pkgs/ts/generated/output

generate: materialize spago-build purs-tsd-gen patchTsTypes

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

ci: generate
	nix -L build .#ts.builds.storybook
