import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const baseConfig = {
  entry: './index.js',
  output: {
    filename: 'index.js',
    path: path.resolve(__dirname, 'static'),
  },
};

export default function (env, argv) {
    let config = {
        ...baseConfig,
        mode: argv.mode || 'development',
        devtool: false,
    };
    return config;
}
