default:
	echo 'Hello, world!'

vscode-toggle-purs-export-lens:
	patch-json '(j) => ({...j, "purescript.exportsCodeLens": !j["purescript.exportsCodeLens"]})' .vscode/settings.json
