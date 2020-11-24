import * as toc from './src/toc.mjs'
import * as fs from 'fs'

toc.files.forEach(function (file) {
  var markdown = fs.readFileSync(file).toString()

  var currentToc = toc.getCurrent(markdown)
  if (currentToc == null) return

  var generatedToc = toc.generate(markdown)
  if (currentToc === generatedToc) return

  fs.writeFileSync(file, markdown.replace(currentToc, generatedToc))
  console.log(`Updated ${file}`)
})
