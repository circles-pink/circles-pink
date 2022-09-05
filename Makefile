PURS_OUTPUT=pkgs/ts/@circles-pink/state-machine/output

all: build-storybook rw-result

dev-browser:
	bash -c '${BROWSER} http://localhost:6006'

build-storybook: assets build-storybook_

build-storybook_:
	export OUTPUT_DIR=../../../dist; yarn workspace storybook run build

yarn-install: clean-install
	yarn install

clean-install:
ifeq ($(PRUNE),true)
	rm -rf node_modules
	rm -rf pkgs/ts/*/node_modules
	rm -rf pkgs/ts/*/*/node_modules
endif

clean-materialized:
ifeq ($(PRUNE),true)
	rm -rf materialized
endif

materialize: clean-materialized spago2nix

spago2nix:
	just spago2nix

clean-generate:
ifeq ($(PRUNE),true)
	rm -rf generated;
endif

spago-clean:
ifeq ($(PRUNE),true)
	rm -f psa-stash && rm -rf .spago $(PURS_OUTPUT)
endif

spago-build: spago-clean
	just spago-build

spago-test: spago-clean yarn-install
	just spago-test

spago-test-watch: spago-clean yarn-install
	spago test --watch --purs-args '--output $(PURS_OUTPUT)'

patchTsTypes: yarn-install 
	patchTsTypes $(PWD)/$(PURS_OUTPUT)

assets: generate assets_

assets_:
	nix build .#assets -o result-assets
	mkdir -p ./pkgs/ts/assets/src
	rm -rf ./pkgs/ts/assets/src/*
	cp -r result-assets/* -t ./pkgs/ts/assets/src
	rm result-assets

rw-result:
	rm -rf rw-result
	cp -r -H --dereference result/ rw-result
	chmod -R 777 rw-result

generate: materialize clean-generate spago-build generate_

generate_:
	make generate-zeus

generate-watch:
	chokidar '$(PURS_OUTPUT)/*/*.js' -c "make generate_"

generate-zeus:
	nix build .#zeus-client --out-link result-zeus-client
	rm -rf ./pkgs/ts/@circles-pink/zeus-client/src
	mkdir -p ./pkgs/ts/@circles-pink/zeus-client/src
	cp -r result-zeus-client/* -t ./pkgs/ts/@circles-pink/zeus-client/src
	chmod -R u+w ./pkgs/ts/@circles-pink/zeus-client/src
	rm result-zeus-client

vm-create:
	nixops create -d circles-pink-vm networks/vm.nix

vm-deploy: agent-get-secrets vm-start
	nixops deploy --allow-reboot -d circles-pink-vm

vm-deploy-dry-watch: agent-get-secrets
	chokidar \
	  --ignore '**/node_modules/**' \
	  --ignore '.spago/**' \
	  --ignore '.git/**' \
	  '**/*.nix' \
	  -c "pkill nixops; log-result 'nixops deploy --dry-run --allow-reboot -d circles-pink-vm'"

vm-deploy-build-watch: agent-get-secrets vm-start
	chokidar \
	  --ignore '**/node_modules/**' \
	  --ignore '.spago/**' \
	  --ignore '.git/**' \
	  '**/*.nix' \
	  -c "pkill nixops; log-result 'nixops deploy --build-only --allow-reboot -d circles-pink-vm'"

vm-ssh:
	nixops ssh -d circles-pink-vm webserver

vm-remove:
	nixops destroy -d circles-pink-vm; nixops delete -d circles-pink-vm

vm-remove-all:
	nixops destroy --all; nixops delete --all

vm-start:
	nixops start -d circles-pink-vm

vm-stop:
	nixops stop -d circles-pink-vm

vm-info:
	nixops info	-d circles-pink-vm

vm-ip:
	nixops info -d circles-pink-vm --plain 2> /dev/null | awk -F'\t' '$$1 == "webserver" {print $$6}'

vm-browser:
	bash -c '${BROWSER} 'http://`make -s --no-print-directory vm-ip`

shell:
	nix develop

agent-get-secrets:
	scp root@hercules.teal.ooo:/var/lib/hercules-ci-agent/secrets/secrets.json .

agent-push-secrets:
	scp ./secrets.json root@hercules.teal.ooo:/var/lib/hercules-ci-agent/secrets/secrets.json;

directus-dump-schema:
	nixops ssh -d circles-pink-vm webserver \
	"rm -rf /tmp/directus-dump; mkdir /tmp/directus-dump; cd /tmp/directus-dump; directus-dump-schema"
	rm -rf ./materialized/directus-dump
	nixops scp -d circles-pink-vm --from webserver /tmp/directus-dump ./materialized

directus-init-db:
	nixops ssh -d circles-pink-vm webserver directus-init-tables

directus-seed-db: directus-init-db directus-seed-db_

directus-seed-db_:
	export DIRECTUS_ADMIN_TOKEN=$$(cat ./secrets.json | jq -r '.secrets.data."directus-admin-token"'); ts-node ./pkgs/ts/seed-db/src/index.ts

ci:
	nix flake check