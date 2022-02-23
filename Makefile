all: dev-storybook build-storybook ci rw-result

dev-storybook: assets
	yarn workspace storybook run storybook

build-storybook: assets
	export OUTPUT_DIR=../../../dist; yarn workspace storybook run build

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;

yarn-install: clean-install
	yarn install

clean-install:
ifeq ($(PRUNE),true)
	rm -rf node_modules
	rm -rf pkgs/ts/*/node_modules
endif

clean-materialized:
ifeq ($(PRUNE),true)
	rm -rf materialized
endif

materialize: clean-materialized spago2nix

clean-generate:
ifeq ($(PRUNE),true)
	rm -rf generated;
endif

spago-clean:
ifeq ($(PRUNE),true)
	rm -rf .spago output
endif

spago-build: spago-clean
	spago build
	mkdir -p generated
	cp -r output -t generated

purs-tsd-gen:
	purs-tsd-gen --directory generated/output

patchTsTypes: yarn-install 
	patchTsTypes $(PWD)/generated/output

assets: generate
	mkdir -p ./pkgs/ts/assets/src/ && \
	node -e 'require("./output/CirclesPink.GenGraph").main()' "./pkgs/ts/assets/src/circles-state-machine.dot" && \
	dot -Tsvg ./pkgs/ts/assets/src/circles-state-machine.dot > ./pkgs/ts/assets/src/circles-state-machine.svg && \
	make -Bnd | make2graph | dot -Tsvg -o ./pkgs/ts/assets/src/circles-makefile.svg

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

generate: materialize clean-generate
	make spago-build 
	make purs-tsd-gen
	make patchTsTypes
	cp -r generated/output pkgs/ts/generated

ci: materialize
	nix -L build .#publicDir
