const path = require('path');

module.exports = function webpackConfig(env, args) {
  return {
    //entry: path.join(__dirname, 'src/index.tsx'),
    // output: {
    //   filename: 'main.js',
    //   path: path.join(__dirname, 'public'),
    // },
    output: {
      path: path.join(__dirname, 'public'),
      filename: 'dist/[name].js',
      sourceMapFilename: 'dist/[name].js.map',
    },
    resolve: {
      extensions: ['.tsx', '.js', '.ts'],
      fallback: {
        os: require.resolve('os-browserify/browser'),
        https: require.resolve('https-browserify'),
        http: require.resolve('stream-http'),
        crypto: require.resolve('crypto-browserify'),
      },
    },
    module: {
      rules: [
        {
          test: /\.[jt]sx?$/,
          exclude: /node_modules/,
          loader: require.resolve('babel-loader'),
          // See .babelrc for further babel config
        },
      ],
    },
    optimization: {
      //   minimizer: [
      //     // Omit creation of .txt files
      //     new (require('terser-webpack-plugin'))({ extractComments: false }),
      //   ],
    },
    devServer: {
      hot: true,
      open: false,
      static: { directory: path.join(__dirname, 'public') },
    },
  };
};
