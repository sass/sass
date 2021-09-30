import * as colors from 'colors/safe';
import * as diff from 'diff';
import * as fs from 'fs';

import * as toc from '../tool/toc';

toc.files.forEach(file => {
  const markdown = fs.readFileSync(file).toString();
  console.log('Reading: ' + file);

  const currentToc = toc.getCurrent(markdown);
  if (currentToc === null) {
    console.log(colors.yellow("File doesn't have a table of contents"));
    return;
  }

  const generatedToc = toc.generate(markdown);
  if (currentToc === generatedToc) return;

  const tocDiff = diff.diffLines(currentToc, generatedToc);
  tocDiff.forEach(part => {
    const color = part.added
      ? colors.green
      : part.removed
      ? colors.red
      : colors.grey;
    process.stderr.write(color(part.value));
  });
  process.stderr.write('\n');

  process.exitCode = 1;
});
