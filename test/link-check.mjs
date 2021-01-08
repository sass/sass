import colors from 'colors/safe.js'
import * as fs from 'fs'
import glob from 'glob'
import toc from 'markdown-toc'
import * as urlModule from 'url'
import markdownLinkCheck from 'markdown-link-check'
import * as path from 'path'

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

  var dirname = path.dirname(urlModule.fileURLToPath(import.meta.url))
  markdownLinkCheck(markdown, {
    retryOn429: true, // Retry if the github rate limit is reached
    baseUrl: path.basename(dirname) + '/'
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
