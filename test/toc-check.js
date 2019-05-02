'use strict';

var colors = require('colors/safe');
var diff = require('diff');
var fs = require("fs");
var glob = require("glob");
var toc = require('markdown-toc');

var files = glob.sync("**/*.md", {ignore: ["node_modules/**/*.md"]})

//Find the begginning index of the Table of Contents
function findStartIndex(str) {
  var startIndex = str.indexOf('Table of Contents');
  while (str.charAt(startIndex) != '*') startIndex += 1;
  return startIndex;
}

//Returns the current Table of Contents
function getCurrentToc(str, remove) {
  if (remove === undefined) remove = 0;
  var startIndex = findStartIndex(str);
  var endIndex = str.indexOf("## ", startIndex) - 1;
  var result = str.substring(startIndex, endIndex);

  return result.trim();
}

files.forEach(function(file) {
  var markdown = fs.readFileSync(file).toString();
  console.log("Reading: " + file);

  if (markdown.includes('Table of Contents')) {
    var generatedToc = toc(markdown, {
      filter: (string, _, __) => string.indexOf('Table of Contents') === -1,
      firsth1: false,
      bullets: "*"
    }).content;
    var currentToc = getCurrentToc(markdown);

    if (currentToc !== generatedToc) {
      var tocDiff = diff.diffLines(currentToc, generatedToc);
      tocDiff.forEach(function(part) {
        var color = part.added
          ? colors.green
          : (part.removed ? colors.red : colors.grey);
        process.stderr.write(color(part.value));
      });

      process.stderr.write('\n');

      process.exitCode = 1;
    }
  } else {
    console.log(colors.yellow("File doesn't have a table of contents"));
  }
});


