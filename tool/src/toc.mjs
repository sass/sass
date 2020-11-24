import glob from 'glob'
import markdownToc from 'markdown-toc'

/// Files that may contain tables of contents.
export const files = glob.sync('**/*.md', {
  ignore: ['node_modules/**/*.md', '**/*.changes.md']
})

// Returns the index of the first `*` in `markdown`'s table of contents, or
// `null` if `markdown` doesn't include a table of contents.
function findStartIndex (markdown) {
  var match = markdown.match(/Table of Contents\n*\*/)
  if (match == null) return null
  return match.index + match[0].length - 1
}

// Returns the table of contents in `markdown` if it contains one, or `null`
// otherwise.
export function getCurrent (markdown) {
  var startIndex = findStartIndex(markdown)
  if (startIndex == null) return null

  var endIndex = markdown.indexOf('## ', startIndex) - 1
  if (endIndex < 0) return null

  return markdown.substring(startIndex, endIndex).trim()
}

/// Returns the expected table of contents for `markdown`.
export function generate (markdown) {
  return markdownToc(markdown, {
    filter: (string, _, __) => string.indexOf('Table of Contents') === -1,
    firsth1: false,
    bullets: '*'
  }).content
}
