{  }:
  allDeps:
    {
      name = "circles-pink";
      version = "1.0.0";
      nodeBuildInputs = let
        a = allDeps;
      in [
        (a."@storybook/addon-essentials@^6.4.14")
        (a."@storybook/addon-links@^6.4.14")
        (a."babel-loader@^8.2.3")
        (a."@storybook/react@^6.4.14")
        (a."@storybook/addon-actions@^6.4.14")
        (a."react-dom@^17.0.2")
        (a."react@^17.0.2")
        (a."@babel/core@^7.16.12")
        ];
      meta = { description = ""; license = ""; homepage = ""; };
      }
