import colors from 'colors/safe.js';
import * as fs from 'fs';
import glob from 'glob';
import markdownLinkCheck from 'markdown-link-check';
import markdownToc from 'markdown-toc';
import * as path from 'path';
import {fileURLToPath, pathToFileURL, URL} from 'url';

const files = glob.sync('**/*.md', {
  ignore: ['node_modules/**/*.md'],
});

const tocCache = new Map();

function getToc(file) {
  file = path.normalize(file);
  if (tocCache.has(file)) {
    return tocCache.get(file);
  } else {
    const result = markdownToc(fs.readFileSync(file).toString(), {}).content;
    tocCache.set(file, result);
    return result;
  }
}

files.forEach((file, i) => {
  const markdown = fs.readFileSync(file).toString();

  const checkOptions = {
    retryOn429: true, // Retry if the github rate limit is reached
    baseUrl: path.basename(path.dirname(fileURLToPath(import.meta.url))) + '/',
  };

  // Rate-limit http requests sent by `markdownLinkCheck` to avoid timeouts.
  setTimeout(
    () => markdownLinkCheck(markdown, checkOptions, handleCheckResult),
    i * 500
  );

  function handleCheckResult(err, results) {
    if (err) {
      console.error('Error', err);
      return;
    }

    console.log('Reading: ' + file);

    // Get a list of all headers so we can verify intra-document links.
    const markdownToc = getToc(file);

    results.forEach(result => {
      const url = new URL(result.link, pathToFileURL(file));
      if (url.protocol === 'file:' && !result.link.match(/ \(.*\)$/)) {
        const target = fileURLToPath(url);
        if (!fs.existsSync(target)) {
          process.exitCode = 1;
          console.log(colors.red(`Missing file: ${result.link}`));
          return;
        }

        if (url.hash === '') return;
        const toc = getToc(target);

        if (toc.includes(url.hash)) return;
        process.exitCode = 1;
        console.log(colors.red(`Dead: ${result.link}`));
        return;
      }

      if (result.link.match(/^#/)) {
        if (markdownToc.includes(result.link)) {
          result.status = 'alive';
        } else {
          result.status = 'dead';
          result.statusCode = 0;
        }
      }

      if (result.status === 'dead') {
        if (result.statusCode === 500) {
          console.log(colors.yellow(`Server error on target: ${result.link}`));
        } else {
          process.exitCode = 1;
          console.log(colors.red(`Dead: ${result.link}`));
        }
      } else if (result.status === 'error') {
        process.exitCode = 1;
        console.log(colors.red(`Error: ${result.link}`));
      }
    });
  }
});
