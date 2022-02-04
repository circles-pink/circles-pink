{ pkgs, scope, engines, ... }:
let
  chromeUser = "Default";

  dirs = [
    "$HOME/.config/chromium"
    "$HOME/snap/chromium/common/chromium"
  ];

  mkName = name: "${name} [${scope}]";

  sqlFileInsert = name: value: ''
    insert into keywords (short_name, keyword, url, favicon_url)
    values ("${mkName name}", "${builtins.elemAt value 0}", "${builtins.elemAt value 1}", "");
  '';

  sqlRemove = "DELETE FROM keywords WHERE short_name LIKE '${mkName "%"}';";

  search-bar = name: sqlScript: pkgs.writeShellScriptBin name (''
    dir_count=0
    killall chrome --wait
  '' + (
    pkgs.lib.concatMapStrings
      (dir: ''
        db_file="${dir}/${chromeUser}/Web Data"

        if [ -f "$db_file" ];
        then

          echo "patching $db_file ..."
          ${pkgs.sqlite}/bin/sqlite3 \
            "$db_file" < ${pkgs.writeText "transaction.sql" sqlScript} 2>&1 \
            | sed 's/^/    sqlite: /'

          if [ ''${PIPESTATUS[0]} -eq 0 ];
          then
            dir_count=$((dir_count+1))
          fi

          echo
        fi
      '')
      dirs
  ) + ''
    if [ "$dir_count" -eq 0 ];
    then
      echo "No databases have been patched"
      exit 1;
    fi
  ''
  );

  update = search-bar "search-bar-update" ''
    ${sqlRemove}

    ${builtins.concatStringsSep "\n" (pkgs.lib.mapAttrsToList sqlFileInsert engines)}
  '';

  remove = search-bar "search-bar-remove" ''
    ${sqlRemove}
  '';


in
{
  inherit update remove;
}
