'use strict'

var colors = require('colors/safe')
var fs = require('fs')
var glob = require('glob')
var toc = require('markdown-toc')
var urlModule = require('url')
var markdownLinkCheck = require('markdown-link-check')
var path = require('path')

var files = glob.sync('**/*.md', { ignore: ['node_modules/**/*.md'] })

var tocCache = new Map()

function getToc (file) {
  file = path.normalize(file)
  if (tocCache.has(file)) {
    return tocCache.get(file)
  } else {
    var result = toc(fs.readFileSync(file).toString()).content
    tocCache.set(file, result)
    return result
  }
}

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
    var markdownToc = getToc(file)

    results.forEach(function (result) {
      var url = new URL(result.link, urlModule.pathToFileURL(file))
      if (url.protocol === 'file:' && !result.link.match(/ \(.*\)$/)) {
        var target = urlModule.fileURLToPath(url)
        if (!fs.existsSync(target)) {
          process.exitCode = 1
          console.log(colors.red(`Missing file: ${result.link}`))
          return
        }

        if (url.hash === '') return
        var toc = getToc(target)

        if (toc.includes(url.hash)) return
        process.exitCode = 1
        console.log(colors.red(`Dead: ${result.link}`))
        return
      }

      if (result.link.match(/^#/)) {
        if (markdownToc.includes(result.link)) {
          result.status = 'alive'
        } else {
          result.status = 'dead'
          result.statusCode = 0
        }
      }

      if (result.status === 'dead') {
        if (result.statusCode === 500) {
          console.log(colors.yellow(`Server error on target: ${result.link}`))
        } else {
          process.exitCode = 1
          console.log(colors.red(`Dead: ${result.link}`))
        }
      } else if (result.status === 'error') {
        process.exitCode = 1
        console.log(colors.red(`Error: ${result.link}`))
      }
    })
  })
})
