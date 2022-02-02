dev-storybook:
	npm run storybook --prefix pkgs/ts

spago2nix:
	mkdir -p materialized;
	spago2nix generate; mv spago-packages.nix -t materialized;

node2nix:
	mkdir -p materialized/node2nix;
	node2nix \
	  --input pkgs/ts/package.json \
	  --lock pkgs/ts/package-lock.json \
	  --composition materialized/node2nix/default.nix \
	  --node-env materialized/node2nix/node-env.nix \
	  --output materialized/node2nix/node-packages.nix

clean-materialized:
	rm -rf materialized

materialize: clean-materialized spago2nix node2nix

generate: spago-build purs-tsd-gen patch-ts-types