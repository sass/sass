import * as glob from 'glob';
import markdownToc = require('markdown-toc');

/** Files that may contain tables of contents. */
export const files = glob.sync('**/*.md', {
  ignore: ['node_modules/**/*.md', '**/*.changes.md'],
});

// Returns the index of the first `*` in `markdown`'s table of contents, or
// `null` if `markdown` doesn't include a table of contents.
function findStartIndex(markdown: string): number | null {
  const match = markdown.match(/\n## Table of Contents\n/);
  if (match === null || match.index === undefined) return null;
  return match.index + match[0].length - 1;
}

/**
 * Returns the table of contents in `markdown` if it contains one, or `null`
 * otherwise.
 */
export function getCurrent(markdown: string): string | null {
  const startIndex = findStartIndex(markdown);
  if (startIndex === null) return null;

  const endIndex = markdown.indexOf('## ', startIndex) - 1;
  if (endIndex < 0) return null;

  return markdown.substring(startIndex, endIndex).trim() || null;
}

/** Returns the expected table of contents for `markdown`. */
export function generate(markdown: string): string {
  return markdownToc(markdown, {
    filter: contents => contents.indexOf('Table of Contents') === -1,
    firsth1: false,
    bullets: '*',
  }).content;
}
