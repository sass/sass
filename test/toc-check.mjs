import colors from 'colors/safe.js'
import diff from 'diff'
import * as fs from 'fs'

import * as toc from '../tool/src/toc.mjs'

toc.files.forEach(function (file) {
  var markdown = fs.readFileSync(file).toString()
  console.log('Reading: ' + file)

  var currentToc = toc.getCurrent(markdown)
  if (currentToc == null) {
    console.log(colors.yellow("File doesn't have a table of contents"))
    return
  }

  var generatedToc = toc.generate(markdown)
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
