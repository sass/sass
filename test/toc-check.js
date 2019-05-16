'use strict'

var colors = require('colors/safe')
var diff = require('diff')
var fs = require('fs')
var glob = require('glob')
var toc = require('markdown-toc')

var files = glob.sync('**/*.md', {
  ignore: ['node_modules/**/*.md', '**/*.changes.md']
})

// Returns the index of the first `*` in `markdown`'s table of contents, or
// `null` if.`markdown` doesn't include a table of contents.
function findStartIndex (markdown) {
  var match = markdown.match(/Table of Contents\n*\*/)
  if (match == null) return null
  return match.index + match[0].length - 1
}

// Returns the table of contents in `markdown` if it contains one, or `null`
// otherwise.
function getCurrentToc (markdown) {
  var startIndex = findStartIndex(markdown)
  if (startIndex == null) return null

  var endIndex = markdown.indexOf('## ', startIndex) - 1
  if (endIndex < 0) return null

  return markdown.substring(startIndex, endIndex).trim()
}

files.forEach(function (file) {
  var markdown = fs.readFileSync(file).toString()
  console.log('Reading: ' + file)

  var currentToc = getCurrentToc(markdown)
  if (currentToc == null) {
    console.log(colors.yellow("File doesn't have a table of contents"))
    return
  }

  var generatedToc = toc(markdown, {
    filter: (string, _, __) => string.indexOf('Table of Contents') === -1,
    firsth1: false,
    bullets: '*'
  }).content
  if (currentToc === generatedToc) return

  var tocDiff = diff.diffLines(currentToc, generatedToc)
  tocDiff.forEach(function (part) {
    var color = part.added
      ? colors.green
      : (part.removed ? colors.red : colors.grey)
    process.stderr.write(color(part.value))
  })
  process.stderr.write('\n')

  process.exitCode = 1
})
