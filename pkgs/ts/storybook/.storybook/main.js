const path = require("path");

module.exports = {
  stories: ["../src/**/*.stories.mdx", "../src/**/*.stories.@(js|jsx|ts|tsx)"],
  addons: [
    "@storybook/addon-links",
    // '@storybook/addon-actions',
    "@storybook/addon-viewport",
    "@storybook/addon-docs",
    "@storybook/addon-controls",
    // "@storybook/addon-backgrounds",
    "@storybook/addon-toolbars",
    "@storybook/addon-measure",
    "@storybook/addon-outline",
  ],
  framework: "@storybook/react",
  webpackFinal: async (config) => {
    config.module.rules = [
      ...config.module.rules,
      {
        test: /\.css$/,
        use: [
          {
            loader: "postcss-loader",
            options: {
              postcssOptions: {
                ident: "postcss",
                plugins: [require("autoprefixer")],
              },
            },
          },
        ],
        include: path.resolve(__dirname, "../"),
      },
      {
        test: /\.(ts|tsx)$/,
        use: [
          {
            loader: require.resolve("babel-loader"),
            options: {
              presets: [require.resolve("babel-preset-react-app")],
            },
          },
          require.resolve("react-docgen-typescript-loader"),
        ],
      },
    ];
    config.resolve.extensions.push(".ts", ".tsx");
    return config;
  },
  typescript: {
    check: true,
    checkOptions: {
      tsconfig: path.resolve(__dirname, "../../../../tsconfig.json"),
    },
    reactDocgen: "react-docgen-typescript",
    reactDocgenTypescriptOptions: {
      shouldExtractLiteralValuesFromEnum: true,
      propFilter: (prop) =>
        prop.parent ? !/node_modules/.test(prop.parent.fileName) : true,
    },
  },
};
