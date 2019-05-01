'use strict';

var toc = require('markdown-toc');
var fs = require("fs");
var glob = require("glob");
var chalk = require("chalk");

var files = glob.sync("**/*.md", {ignore: ["node_modules/**/*.md"]})

function removeTocHeader(str, ele, arr) {
  return str.indexOf('Table of Contents') === -1;
}

function getCurrentToc(str){
  var startIndex = str.indexOf('Table of Contents') + 19;
  var endIndex = str.indexOf("## ", startIndex) - 2;
  var result = str.substring(startIndex, endIndex);
  return result;
}

files.forEach(function(file) {
  var markdown = fs.readFileSync(file).toString();
  console.log("Reading: " + file);

  if(markdown.includes('Table of Contents')){
    var generatedToc = toc(markdown, {filter: removeTocHeader, firsth1: false, bullets: "*"}).content;
    var currentToc = getCurrentToc(markdown);

    

    if(currentToc !== generatedToc) {
      console.log(generatedToc.length);
      console.log(chalk.green("Expected TOC: \n"+ generatedToc));
      console.log(currentToc.length)
      console.log(chalk.red("Current TOC: \n"+ currentToc));
    }
  }else{
    console.log(chalk.yellow("File doesn't have a TOC"));
  }
  
});


