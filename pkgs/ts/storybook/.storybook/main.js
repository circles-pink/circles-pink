const path = require('path')

module.exports = {
  stories: [
    "../src/stories/**/*.stories.mdx",
    "../src/stories/**/*.stories.@(js|jsx|ts|tsx)"
  ],
  addons: [
    "@storybook/addon-links",
    "@storybook/addon-docs",
    "@storybook/addon-essentials",
    '@storybook/addon-actions',
    '@storybook/addon-knobs',
    '@storybook/addon-notes',
    {
      name: '@storybook/addon-postcss',
      options: {
        postcssLoaderOptions: {
          implementation: require('postcss'),
        },
      },
    },
  ],
  // webpackFinal: async config => {
  //   config.module.rules = [
  //     ...config.module.rules,
  //     {
  //       test: /\.(ts|tsx)$/,
  //       use: [
  //         {
  //           loader: require.resolve("babel-loader"),
  //           options: {
  //             presets: [require.resolve("babel-preset-react-app")]
  //           }
  //         }
  //         //require.resolve("react-docgen-typescript-loader")
  //       ]
  //     },
  //     {
  //       test: /\.css$/,
  //       use: [
  //         {
  //           loader: 'postcss-loader',
  //           options: {
  //             postcssOptions: {
  //               ident: 'postcss',
  //               plugins: [
  //                 require('tailwindcss'),
  //                 require('autoprefixer'),
  //               ],
  //             },
  //           },
  //         },
  //       ],
  //       include: path.resolve(__dirname, '../'),
  //     },
  //   ],
  //     config.resolve.extensions.push('.ts', '.tsx');
  //   return config;
  // },
};