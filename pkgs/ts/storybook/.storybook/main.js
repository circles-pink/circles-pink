module.exports = {
  stories: [
    "../stories/**/*.stories.mdx",
    "../stories/**/*.stories.@(js|jsx|ts|tsx)"
  ],
  addons: [
    "@storybook/addon-links",
    "@storybook/addon-essentials"
  ],
  framework: "@storybook/react",
  webpackFinal: async config => {
    config.module.rules = [
      ...config.module.rules,
      {
        test: /\.(ts|tsx)$/,
        use: [
          {
            loader: require.resolve("babel-loader"),
            options: {
              presets: [require.resolve("babel-preset-react-app")]
            }
          },
          require.resolve("react-docgen-typescript-loader")
        ]
      },
    ];
    config.resolve.extensions.push('.ts', '.tsx');
    return config;
  },
}