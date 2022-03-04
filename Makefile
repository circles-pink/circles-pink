PURS_OUTPUT=pkgs/ts/generated/output

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
	rm -rf .spago $(PURS_OUTPUT)
endif

spago-build: spago-clean
	mkdir -p generated
	spago build --purs-args '--output $(PURS_OUTPUT)'

spago-test: spago-clean yarn-install
	mkdir -p generated
	spago test --purs-args '--output $(PURS_OUTPUT)'

spago-test-watch: spago-clean yarn-install
	mkdir -p generated
	spago test --watch --purs-args '--output $(PURS_OUTPUT)'

purs-tsd-gen:
	purs-tsd-gen --directory $(PURS_OUTPUT)

patchTsTypes: yarn-install 
	patchTsTypes $(PWD)/$(PURS_OUTPUT)

assets: generate
	nix build .#assets -o ./pkgs/ts/assets/src

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

generate: materialize clean-generate
	make spago-build 
	make purs-tsd-gen
	make patchTsTypes

ci: materialize
	nix -L build .#publicDir