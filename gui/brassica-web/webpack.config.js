import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const baseConfig = {
  entry: {
    index: './src/index.js',
    builder: './src/builder.js',
  },
  devServer: {
    static: './dist',
  },
  output: {
    filename: '[name].js',
    path: path.resolve(__dirname, 'dist'),
  },
};

export default function (env, argv) {
    let config = baseConfig;
    if (argv.mode === 'development') {
        config.mode = 'development';
        config.devtool = 'inline-source-map';
    } else {
        config.mode = 'production';
        config.devtool = 'source-map';
    }
    return config;
}
