var path = require('path');
var webpack = require('webpack');
 
module.exports = {
    mode: 'production',
    entry: { 'covidUsaCanada': './src/index.tsx' },
    module: {
        rules: [{
            test: /\.(ts|js)x?$/,
            // All node modules, including three.
            exclude: /node_modules/,
            use: { loader: 'babel-loader' }
        }]
    },
    stats: {
        colors: true
    },
    devtool: 'source-map',
    resolve: {
        extensions: [".ts", ".tsx", ".js", ".jsx" ]
    },
    plugins: [],
    output: {
        filename: `[name].bundle.js`
    }
};
