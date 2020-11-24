import * as toc from './src/toc.mjs'
import * as fs from 'fs'

toc.files.forEach(function (file) {
  var markdown = fs.readFileSync(file).toString()

  var currentToc = toc.getCurrent(markdown)
  if (currentToc == null) {
    var match = markdown.match('## Table of Contents\n\n')
    if (!match) return

    var tocLocation = match.index + match[0].length

    // If there's an empty TOC, fill it in.
    fs.writeFileSync(
      file,
      markdown.substring(0, tocLocation) +
        toc.generate(markdown) + '\n\n' +
        markdown.substring(tocLocation))
    console.log(`Added TOC to ${file}`)
    return
  }

  var generatedToc = toc.generate(markdown)
  if (currentToc === generatedToc) return

  fs.writeFileSync(file, markdown.replace(currentToc, generatedToc))
  console.log(`Updated TOC in ${file}`)
})
