const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');

const path = require('path');
const isDev = process.env.NODE_ENV === 'development';
const isProd = process.env.NODE_ENV === 'production';

const projectName = "FableTranspiler.WebClient";

module.exports = {
    context: path.resolve(__dirname, projectName),
    entry: './' + projectName + '.fsproj',
    output: {
        filename: '[name].[chunkhash].bundle.js',
        path: path.resolve(__dirname, 'public'),
        publicPath: isProd ? '' : '/' // if '/' when build then NO RESOURCE FOUND
    },
    devServer: {
        port: 4200,
        contentBase: path.resolve(__dirname, 'public'),
        publicPath: '/',
        historyApiFallback: true,
        hot: isDev
    },
    plugins: [
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
            title: 'Fable-React Course',
            cache: false,
            template: './assets/index.html',
            minify: {
                collapseWhitespace: isProd
            }
        })
    ],
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: 'fable-loader',
            },
            {
                test: /\.css$/,
                use: [ 'style-loader', 'css-loader', 'postcss-loader' ]
            },
            {
                test: /\.(png|jpg|svg|gif)$/,
                use: ['file-loader']
            },
            {
                test: /\.(ttf|woff|woff2|eot)$/,
                use: ['file-loader']
            }
        ]
    },
    optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },
    devtool: isDev ? 'source-map' : false
}