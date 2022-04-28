default:
	echo 'Hello, world!'

vscode-toggle-purs-export-lens:
	patch-json '(j) => ({...j, "purescript.exportsCodeLens": !j["purescript.exportsCodeLens"]})' .vscode/settings.json

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
