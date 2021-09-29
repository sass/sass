import * as fs from 'fs';

import * as toc from './toc';

toc.files.forEach(file => {
  const markdown = fs.readFileSync(file).toString();

  const currentToc = toc.getCurrent(markdown);
  if (currentToc === null) {
    const match = markdown.match('## Table of Contents\n\n');
    if (!match || !match.index) return;

    const tocLocation = match.index + match[0].length;

    // If there's an empty TOC, fill it in.
    fs.writeFileSync(
      file,
      markdown.substring(0, tocLocation) +
        toc.generate(markdown) +
        '\n\n' +
        markdown.substring(tocLocation)
    );
    console.log(`Added TOC to ${file}`);
    return;
  }

  const generatedToc = toc.generate(markdown);
  if (currentToc === generatedToc) return;

  fs.writeFileSync(file, markdown.replace(currentToc, generatedToc));
  console.log(`Updated TOC in ${file}`);
});
