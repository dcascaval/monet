const path = require('path')

module.exports = {
  entry: './index.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolve: {
    fallback: {
      "path": require.resolve("path-browserify"),
      "crypto": require.resolve("crypto-browserify"),
      "buffer": require.resolve("buffer/"),
      "fs": require.resolve("browserify-fs"),
      "stream": require.resolve("stream-browserify"),
      "util": require.resolve("util/")
    }
  }
}