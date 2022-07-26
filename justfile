set dotenv-load
set positional-arguments

export PATH := "./node_modules/.bin:" + env_var('PATH')

CIRCLES_DEV := env_var_or_default('CIRCLES_DEV', 'dev')
CIRCLES_DEV_OS := env_var_or_default('CIRCLES_DEV_OS', '')
CIRCLES_TOOLBELT_PATH := env_var_or_default('CIRCLES_TOOLBELT_PATH', 'checkouts/circles-toolbelt')
GARDEN_PATH := env_var_or_default('GARDEN_PATH', 'checkouts/circles-docker')
VOUCHER_CODE_SECRET := env_var_or_default('VOUCHER_CODE_SECRET', "0")
XBGE_AUTH_SECRET := env_var_or_default('XBGE_AUTH_SECRET', "0")
XBGE_ENDPOINT := env_var_or_default('XBGE_ENDPOINT', "0")
XBGE_SAFE_ADDRESS := env_var_or_default('XBGE_SAFE_ADDRESS', "0")
XBGE_KEY := env_var_or_default('XBGE_KEY', "0")

TASKS_EXPLORER_SERVER := env_var_or_default("TASKS_EXPLORER_SERVER", "http://tasks.circles.local")
DIRECTUS_URL := env_var_or_default("DIRECTUS_URL", "http://directus.circles.local/graphql")
VOUCHER_SERVER_HOST := env_var_or_default("VOUCHER_SERVER_HOST", "http://localhost:3000")
GARDEN_API := env_var_or_default("GARDEN_API", "http://api.circles.local")
GARDEN_API_USERS := env_var_or_default("GARDEN_API_USERS", "http://api.circles.local/api/users")
GARDEN_GRAPH_API := env_var_or_default("GARDEN_GRAPH_API", "http://graph.circles.local")
GARDEN_SUBGRAPH_NAME := env_var_or_default("GARDEN_SUBGRAPH_NAME", "CirclesUBI/circles-subgraph")
GARDEN_RELAY := env_var_or_default("GARDEN_RELAY", "http://relay.circles.local")
GARDEN_HUB_ADDRESS := env_var_or_default("GARDEN_HUB_ADDRESS", "0xCfEB869F69431e42cdB54A4F4f105C19C080A601")
GARDEN_PROXY_FACTORY_ADRESS := env_var_or_default("GARDEN_PROXY_FACTORY_ADRESS", "0xD833215cBcc3f914bD1C9ece3EE7BF8B14f841bb")
GARDEN_SAFE_MASTER_ADDRESS := env_var_or_default("GARDEN_SAFE_MASTER_ADDRESS", "0xC89Ce4735882C9F0f0FE26686c53074E09B0D550")
GARDEN_ETHEREUM_NODE_WS := env_var_or_default("GARDEN_ETHEREUM_NODE_WS", "ws://localhost:8545")

VOUCHER_SERVER_BASIC_AUTH := env_var_or_default("VOUCHER_SERVER_BASIC_AUTH", "")

PURS_OUTPUT := "pkgs/ts/@circles-pink/state-machine/output"
PURS_TARGET_PACKAGE := "pkgs/ts/@circles-pink/state-machine"

default:
	nix build .#ci --out-link results/ci/result

# This is not perfect yet. After running this a rebuild is instant. However, if a change is made to the codebase parts need to be rebuilt.
# We don't know the reasons yet.
nix-not-yet-fully-safe-collect-garbage: default
	nix-collect-garbage

version:
	cat package.json | jq '.version'

vscode-toggle-purs-export-lens:
	patch-json '(j) => ({...j, "purescript.exportsCodeLens": !j["purescript.exportsCodeLens"]})' .vscode/settings.json

################################################################################
# branchless
################################################################################

branchless-get:
	[ $(git branch --show-current) == "$CIRCLES_DEV" ]
	git fetch
	git rebase origin/main

branchless-diff:
	[ $(git branch --show-current) == "$CIRCLES_DEV" ]
	git fetch
	git diff origin/main

branchless-put: branchless-get branchless-put_

branchless-put_:
	[ $(git branch --show-current) == "$CIRCLES_DEV" ]
	[ -z "$(git status --porcelain)" ]
	git push --force
	gh pr create --base main --body "" --title "Branchless Update"
	gh pr merge --auto --rebase

################################################################################

release: branchless-get
	bump-npm-versions "patch"
	just branchless-put

branchless-to-main:
	gh pr create --head branchless --base main --title "branchless to main"
	gh pr merge --rebase branchless

circles-garden:
	nix develop .#garden --command \
	"bash" "-c" "cd {{GARDEN_PATH}} && make down && make up EXPOSE_PORTS=1 && make contracts && make subgraph && make up"

circles-garden-restart:
	nix develop .#garden --command \
	"bash" "-c" "cd {{GARDEN_PATH}} && make up EXPOSE_PORTS=1"

circles-garden-fund-safe addr:
	cd {{CIRCLES_TOOLBELT_PATH}}/helper-tools && \
	npm install && \
	node fund-safe.js {{addr}} \

checkouts:
	DIR=checkouts; \
	OUT=result-checkouts; \
	rm -rf $DIR && \
	nix build .#checkouts --out-link $OUT && \
	cp -r $OUT/. $DIR && \
	chmod -R +w $DIR && \
	rm -rf $OUT; \

ci:
	nix -L flake check 2>&1 | sed -E "s#/nix/store/[^/]+/#./#g"

dev-storybook: vm-deploy assets dev-storybook_

dev-storybook_:
	#!/usr/bin/env bash
	set -euxo pipefail
	export STORYBOOK_TASKS_EXPLORER_SERVER={{TASKS_EXPLORER_SERVER}}
	export STORYBOOK_DIRECTUS_URL={{DIRECTUS_URL}}
	export STORYBOOK_GARDEN_API={{GARDEN_API}}
	export STORYBOOK_GARDEN_API_USERS={{GARDEN_API_USERS}}
	export STORYBOOK_GARDEN_GRAPH_API={{GARDEN_GRAPH_API}}
	export STORYBOOK_GARDEN_SUBGRAPH_NAME={{GARDEN_SUBGRAPH_NAME}}
	export STORYBOOK_GARDEN_RELAY={{GARDEN_RELAY}}
	export STORYBOOK_GARDEN_HUB_ADDRESS={{GARDEN_HUB_ADDRESS}}
	export STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS={{GARDEN_PROXY_FACTORY_ADRESS}}
	export STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS={{GARDEN_SAFE_MASTER_ADDRESS}}
	export STORYBOOK_GARDEN_ETHEREUM_NODE_WS={{GARDEN_ETHEREUM_NODE_WS}}
	export STORYBOOK_VOUCHER_SERVER_HOST={{VOUCHER_SERVER_HOST}}
	export STORYBOOK_XBGE_SAFE_ADDRESS={{XBGE_SAFE_ADDRESS}}
	export STORYBOOK_IS_DEV=true
	yarn workspace storybook run storybook

dev-voucher-server:
	#!/usr/bin/env bash
	set -euxo pipefail
	export PORT="4000"
	export GARDEN_API={{GARDEN_API}}
	export GARDEN_API_USERS={{GARDEN_API_USERS}}
	export GARDEN_GRAPH_API={{GARDEN_GRAPH_API}}
	export GARDEN_SUBGRAPH_NAME={{GARDEN_SUBGRAPH_NAME}}
	export GARDEN_RELAY={{GARDEN_RELAY}}
	export GARDEN_HUB_ADDRESS={{GARDEN_HUB_ADDRESS}}
	export GARDEN_PROXY_FACTORY_ADRESS={{GARDEN_PROXY_FACTORY_ADRESS}}
	export GARDEN_SAFE_MASTER_ADDRESS={{GARDEN_SAFE_MASTER_ADDRESS}}
	export GARDEN_ETHEREUM_NODE_WS={{GARDEN_ETHEREUM_NODE_WS}}
	export VOUCHER_CODE_SECRET={{VOUCHER_CODE_SECRET}}
	export XBGE_AUTH_SECRET={{XBGE_AUTH_SECRET}}
	export XBGE_ENDPOINT={{XBGE_ENDPOINT}}
	export XBGE_SAFE_ADDRESS={{XBGE_SAFE_ADDRESS}}
	export XBGE_KEY={{XBGE_KEY}}
	export VOUCHER_SERVER_BASIC_AUTH={{VOUCHER_SERVER_BASIC_AUTH}}
	node ./pkgs/ts/@circles-pink/state-machine/bin/voucher-server

cors-proxy-server:
	ts-node ./pkgs/ts/cors-proxy

_dump:
	just --unstable --dump --dump-format json | jq

run-garden-nix:
	nix build --out-link run-garden .#runGarden && ./run-garden/bin/run-garden; rm run-garden

run-garden: yarn-install run-garden_

run-garden_:
	#!/usr/bin/env bash
	set -euxo pipefail
	export GARDEN_API={{GARDEN_API}}
	export GARDEN_API_USERS={{GARDEN_API_USERS}}
	export GARDEN_GRAPH_API={{GARDEN_GRAPH_API}}
	export GARDEN_SUBGRAPH_NAME={{GARDEN_SUBGRAPH_NAME}}
	export GARDEN_RELAY={{GARDEN_RELAY}}
	export GARDEN_HUB_ADDRESS={{GARDEN_HUB_ADDRESS}}
	export GARDEN_PROXY_FACTORY_ADRESS={{GARDEN_PROXY_FACTORY_ADRESS}}
	export GARDEN_SAFE_MASTER_ADDRESS={{GARDEN_SAFE_MASTER_ADDRESS}}
	export GARDEN_ETHEREUM_NODE_WS={{GARDEN_ETHEREUM_NODE_WS}}
	export VOUCHER_SERVER_HOST={{VOUCHER_SERVER_HOST}}
	export IS_DEV=true
	just spago-build && node -e 'require("./{{PURS_OUTPUT}}/CirclesPink.Garden.ApiScript").main()'

generate-tsd:
	node pkgs/ts/@circles-pink/state-machine/bin/generate-tsd.js --output-dir "{{PURS_OUTPUT}}"
	prettier --write '{{PURS_OUTPUT}}/Data.IxGraph/index.d.ts'
	prettier --write '{{PURS_OUTPUT}}/CirclesPink.Data.Address/index.d.ts'
	prettier --write '{{PURS_OUTPUT}}/CirclesPink.Data.TrustNode/index.d.ts'
	prettier --write '{{PURS_OUTPUT}}/CirclesPink.Data.TrustConnection/index.d.ts'
	

purs-docs:
	LINK=results/purs-docs/result; \
	nix build .#purs.docs --out-link $LINK && \
	miniserve --index index.html $LINK/

################################################################################
# spago
################################################################################

spago-clean-output:
	purs-output-cleaner --output {{PURS_OUTPUT}}

spago-build: spago-clean spago-clean-output
	spago build --purs-args "--stash --censor-lib --output {{PURS_OUTPUT}}"

spago-build-ide: spago-clean spago-clean-output
	spago build --purs-args "--stash --censor-lib --json-errors --output {{PURS_OUTPUT}}"

spago2nix:
	spago2nix-extra --pkgs-dir ./pkgs/purs --target-dir ./materialized/spago2nix

spago-test: spago-clean yarn-install
	just spago-test_

spago-test_:
	for path in "pkgs/purs/"*"/test/Main.purs"; do \
	  TEST_MAIN_MODULE=`cat $path | grep '^module' | sed -E 's/module ([^ ]*) .*/\1/'`; \
	  spago test --main "$TEST_MAIN_MODULE" --purs-args '--output {{PURS_OUTPUT}}' || exit 1; \
	done \

spago-test-fast:
	spago test --main "Test.AllTests.Main" --purs-args '--output {{PURS_OUTPUT}}'

spago-docs:
	ln -fs {{PURS_OUTPUT}} output
	spago docs

doc-search:
	just spago-docs; purescript-docs-search

spago-repl:
  export NODE_PATH={{PURS_TARGET_PACKAGE}}/node_modules; expect ./repl.exp

################################################################################
# ts
################################################################################

ts-build:
	cd pkgs/ts/@circles-pink/web-client; tsc; \
	cd ..; cd ..; cd storybook; tsc --build tsconfig.json

################################################################################

purs-tsd-gen:
	DIR=`mktemp -d`; \
	mv {{PURS_OUTPUT}}/CirclesPink.EnvVars -t $DIR ; \
	mv {{PURS_OUTPUT}}/CirclesPink.Garden.ApiScript -t $DIR ; \
	mv {{PURS_OUTPUT}}/CirclesPink.Garden.TS -t $DIR ; \
	mv {{PURS_OUTPUT}}/Test.VoucherServer.Main -t $DIR; \
	mv {{PURS_OUTPUT}}/Test.CirclesPinkStateMachine.Main -t $DIR; \
	mv {{PURS_OUTPUT}}/Test.AllTests.Main -t $DIR; \
	mv {{PURS_OUTPUT}}/Payload.* {{PURS_OUTPUT}}/VoucherServer.* {{PURS_OUTPUT}}/CirclesPink.Garden.EnvControlAff -t $DIR ; \
	mv $DIR/VoucherServer.Spec.Types -t {{PURS_OUTPUT}} ; \
	purs-tsd-gen --directory {{PURS_OUTPUT}} ; \
	mv $DIR/* -t {{PURS_OUTPUT}} ; \

################################################################################

increase-file-watchers:
	sudo sysctl fs.inotify.max_user_watches=327680 && sudo sysctl -p

apps-on-port port:
	sudo lsof -t -i:{{port}}

################################################################################
# All Makefile tasks
################################################################################

dev-browser:
	make dev-browser

build-storybook:
	make build-storybook

build-storybook_:
	make build-storybook_

yarn-install:
	make yarn-install

clean-install:
	make clean-install

clean-materialized:
	make clean-materialized

materialize:
	make materialize

clean-generate:
	make clean-generate

spago-clean:
	make spago-clean

spago-test-watch:
	make spago-test-watch

patchTsTypes:
	make patchTsTypes

assets:
	make assets

assets_:
	make assets_

rw-result:
	make rw-result

generate:
	make generate

generate_:
	make generate_

generate-watch:
	make generate-watch

generate-zeus:
	make generate-zeus

vm-create:
	make vm-create

vm-deploy:
	make vm-deploy

vm-deploy-dry-watch:
	make vm-deploy-dry-watch

vm-deploy-build-watch:
	make vm-deploy-build-watch

vm-ssh:
	make vm-ssh

vm-remove:
	make vm-remove

vm-remove-all:
	make vm-remove-all

vm-start:
	make vm-start

vm-stop:
	make vm-stop

vm-info:
	make vm-info

vm-ip:
	make vm-ip

vm-browser:
	make vm-browser

shell:
	make shell

agent-get-secrets:
	make agent-get-secrets

agent-push-secrets:
	make agent-push-secrets

directus-dump-schema:
	make directus-dump-schema

directus-init-db:
	make directus-init-db

directus-seed-db:
	make directus-seed-db

directus-seed-db_:
	make directus-seed-db_