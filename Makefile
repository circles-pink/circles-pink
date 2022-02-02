dev-storybook:
	npm run storybook --prefix pkgs/ts

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;