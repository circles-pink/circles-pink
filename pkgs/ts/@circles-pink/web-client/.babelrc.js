module.exports = {
  presets: [
    [
      // 'next/babel',
      // {
      //   'preset-react': {
      //     runtime: 'automatic',
      //     importSource: '@emotion/react',
      //   },
      // },
      '@babel/preset-typescript',
    ],
  ],
  plugins: ['@emotion/babel-plugin', 'babel-plugin-macros'],
};
