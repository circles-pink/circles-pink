set dotenv-load

export PATH := "./node_modules/.bin:" + env_var('PATH')

CIRCLES_DEV := env_var_or_default('CIRCLES_DEV', 'dev')
CIRCLES_DEV_OS := env_var_or_default('CIRCLES_DEV_OS', '')
CIRCLES_TOOLBELT_PATH := env_var_or_default('CIRCLES_TOOLBELT_PATH', 'checkouts/circles-toolbelt')
GARDEN_PATH := env_var_or_default('GARDEN_PATH', 'checkouts/circles-docker')

TASKS_EXPLORER_SERVER := env_var_or_default("TASKS_EXPLORER_SERVER", "http://tasks.circles.local")
DIRECTUS_URL := env_var_or_default("DIRECTUS_URL", "http://directus.circles.local/graphql")
GARDEN_API := env_var_or_default("GARDEN_API", "http://api.circles.local")
GARDEN_API_USERS := env_var_or_default("GARDEN_API_USERS", "http://api.circles.local/api/users")
GARDEN_GRAPH_API := env_var_or_default("GARDEN_GRAPH_API", "http://graph.circles.local")
GARDEN_SUBGRAPH_NAME := env_var_or_default("GARDEN_SUBGRAPH_NAME", "CirclesUBI/circles-subgraph")
GARDEN_RELAY := env_var_or_default("GARDEN_RELAY", "http://relay.circles.local")
GARDEN_HUB_ADDRESS := env_var_or_default("GARDEN_HUB_ADDRESS", "0xCfEB869F69431e42cdB54A4F4f105C19C080A601")
GARDEN_PROXY_FACTORY_ADRESS := env_var_or_default("GARDEN_PROXY_FACTORY_ADRESS", "0xD833215cBcc3f914bD1C9ece3EE7BF8B14f841bb")
GARDEN_SAFE_MASTER_ADDRESS := env_var_or_default("GARDEN_SAFE_MASTER_ADDRESS", "0xC89Ce4735882C9F0f0FE26686c53074E09B0D550")
GARDEN_ETHEREUM_NODE_WS := env_var_or_default("GARDEN_ETHEREUM_NODE_WS", "ws://localhost:8545")

PURS_OUTPUT := "pkgs/ts/@circles-pink/state-machine/output"

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
	"bash" "-c" "cd {{GARDEN_PATH}} && make down && make clean && make up && make contracts && make subgraph"

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
	yarn workspace storybook run storybook

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
	just spago-build && node -e 'require("./{{PURS_OUTPUT}}/CirclesPink.Garden.ApiScript").main()'

purs-docs:
	LINK=results/purs-docs/result; \
	nix build .#purs.docs --out-link $LINK && \
	miniserve --index index.html $LINK/

################################################################################
# spago
################################################################################

spago-build: spago-clean
	mkdir -p generated
	spago build --purs-args "--stash --censor-lib --output {{PURS_OUTPUT}}"

spago2nix:
	spago2nix-extra --pkgs-dir ./pkgs/purs --target-dir ./materialized/spago2nix

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

spago-test:
	make spago-test

spago-test-watch:
	make spago-test-watch

purs-tsd-gen:
	make purs-tsd-gen

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