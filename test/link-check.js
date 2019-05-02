'use strict';

var colors = require('colors/safe');
var fs = require("fs");
var glob = require("glob");
var markdownLinkCheck = require('markdown-link-check');
var path = require("path");

var files = glob.sync("**/*.md", {ignore: ["node_modules/**/*.md"]})

files.forEach(function(file) {
  var markdown = fs.readFileSync(file).toString();

  markdownLinkCheck(markdown, {
    baseUrl: path.basename(__dirname) + "/"
  }, function(err, results) {
    if (err) {
      console.error('Error', err);
      return;
    }

    console.log("Reading: " + file);

    results.forEach(function(result) {
      if (result.status === "dead") {
        if (result.statusCode == 500) {
          console.log(colors.yellow(`Server error on target: ${result.link}`));
        } else {
          process.exitCode = 1;
          console.log(colors.red(`Dead: ${result.link}`));
        }
      }
    });
  });
});
