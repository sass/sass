#!/usr/bin/env node

'use strict';

var markdownLinkCheck = require('markdown-link-check');
var fs = require("fs");
var glob = require("glob");
var path = require("path");
var chalk = require("chalk");

var files = glob.sync("**/*.md", {ignore: ["node_modules/**/*.md"]})

var opts = JSON.parse(fs.readFileSync("test/.markdown-link-check.json"));

files.forEach(function(file) {
  var markdown = fs.readFileSync(file).toString();
  opts.baseUrl = path.basename(__dirname) + "/"

  markdownLinkCheck(markdown, opts, function (err, results) {
    if (err) {
        console.error('Error', err);
        return;
    }

    console.log(chalk.green("Reading: " + file));

    results.forEach(function (result) {
      if(result.status === "dead") {
        if (result.statusCode == 500) {
          console.log(chalk.yellow("Server error on target: " + result.link));
        }
        else {
          process.exitCode = 1
          console.log(chalk.red("Dead: " + result.link));
        }
      }
    });
  });
});