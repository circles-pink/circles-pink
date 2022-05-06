vscode-toggle-purs-export-lens:
	patch-json '(j) => ({...j, "purescript.exportsCodeLens": !j["purescript.exportsCodeLens"]})' .vscode/settings.json

branchless-get:
	[ $(git branch --show-current) == "$CIRCLES_DEV" ]
	git fetch
	git rebase origin/main

branchless-put: branchless-get branchless-put_

branchless-put_:
	[ $(git branch --show-current) == "$CIRCLES_DEV" ]
	[ -z "$(git status --porcelain)" ]
	bump-npm-versions
	git stage .
	git commit -m "Bump package versions"
	git push --force
	gh pr create --base main --body "" --title "Branchless Update"
	gh pr merge --auto --rebase 
	

branchless-to-main:
	gh pr create --head branchless --base main --title "branchless to main"
	gh pr merge --rebase branchless

circles-garden:
	cd $GARDEN_PATH && \
	nix-shell -p nodejs-12_x --command \
	"make clean && make up && make contracts && make subgraph"

circles-garden-fund-safe addr:
	cd $CIRCLES_TOOLBELT_PATH/helper-tools
	npm install
	node fund-safe.js {{addr}}


ci:
	nix -L flake check 2>&1 | sed -E "s#/nix/store/[^/]+/#./#g"

# All Makefile tasks

dev-storybook:
	make dev-storybook

dev-storybook_:
	make dev-storybook_

dev-browser:
	make dev-browser

build-storybook:
	make build-storybook

build-storybook_:
	make build-storybook_

spago2nix:
	make spago2nix

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

spago-build:
	make spago-build

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

run-garden-nix:
	make run-garden-nix

run-garden:
	make run-garden

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