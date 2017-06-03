'use strict';

var webpack = require('webpack');
var BowerWebpackPlugin = require('bower-webpack-plugin');
//var path = require('path');

var bowerWebpackPlugin = new BowerWebpackPlugin({moduleDirectories: ['./bower_components']});

var config = {
  entry: {
    'chat': './src/Main'
  },
  devtool: 'source-map', //'eval', // 'source-map',
  //devServer: {
  //  contentBase: '.',
  //  port: 9876,
  //  stats: 'errors-only',
  //  historyApiFallback: true
  //},
  output: {
    path: __dirname,
    library: '[name]',
    pathinfo: true,
    filename: '[name].bundle.js'
  },
  plugins: [
    bowerWebpackPlugin,
    //new webpack.ProvidePlugin({'window.Strophe': 'strophejs', 'Strophe': 'strophejs'})
  ],
  module: {
    loaders: [{
      test: /\.purs$/,
      loader: "purs-loader",
      query: {
        src: [ 'bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs', 'examples/**/*.purs' ],
        bundle: false,
        output: './output',
        psc: 'psa',
        pscIde: true,
        pscIdeArgs: {'port': 4042}
      }
    }, {
      test: /\.js$/,
      loader: 'babel-loader',
      exclude: [/hls.js/]
    }]
  },
  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    extensions: [ '.purs', '.js'],
    plugins: [bowerWebpackPlugin]
  }
};

module.exports = config;
