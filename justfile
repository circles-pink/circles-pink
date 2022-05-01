default:
	echo 'Hello, world!'

vscode-toggle-purs-export-lens:
	patch-json '(j) => ({...j, "purescript.exportsCodeLens": !j["purescript.exportsCodeLens"]})' .vscode/settings.json

branchless-get:
	git fetch
	git rebase origin/branchless

branchless-put: branchless-get branchless-put_

branchless-put_:
	git push --force
	gh pr create --base branchless --body "" --title "Branchless Update"
	gh pr merge --auto --rebase

generate_:
	make generate_

dev-storybook:
	make dev-storybook

dev-storybook_:
	make dev-storybook_

dev-browser:
	make dev-browser

build-storybook:
	make build-storybook

circles-garden:
  cd $GARDEN_PATH && \
  make clean && make up && make contracts && make subgraph
