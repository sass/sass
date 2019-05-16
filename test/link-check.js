'use strict'

var colors = require('colors/safe')
var fs = require('fs')
var glob = require('glob')
var toc = require('markdown-toc')
var markdownLinkCheck = require('markdown-link-check')
var path = require('path')

var files = glob.sync('**/*.md', { ignore: ['node_modules/**/*.md'] })

files.forEach(function (file) {
  var markdown = fs.readFileSync(file).toString()

  markdownLinkCheck(markdown, {
    baseUrl: path.basename(__dirname) + '/'
  }, function (err, results) {
    if (err) {
      console.error('Error', err)
      return
    }

    console.log('Reading: ' + file)

    // Get a list of all headers so we can verify intra-document links.
    var markdownToc = toc(markdown).content

    results.forEach(function (result) {
      if (result.link.match(/^#/) && !markdownToc.includes(result.link)) {
        result.status = 'dead'
        result.statusCode = 0
      }

      if (result.status === 'dead') {
        if (result.statusCode === 500) {
          console.log(colors.yellow(`Server error on target: ${result.link}`))
        } else {
          process.exitCode = 1
          console.log(colors.red(`Dead: ${result.link}`))
        }
      }
    })
  })
})
