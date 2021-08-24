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
  let toc = tocCache.get(file);
  if (toc === undefined) {
    toc = markdownToc(fs.readFileSync(file).toString(), {}).content;
    tocCache.set(file, toc);
  }
  return toc;
}

function verifyLinkCheckResults(file, results) {
  const toc = getToc(file);

  for (const result of results) {
    const url = new URL(result.link, pathToFileURL(file));

    // A link to another file.
    if (url.protocol === 'file:' && !result.link.match(/ \(.*\)$/)) {
      const target = fileURLToPath(url);
      if (!fs.existsSync(target)) throw Error(`Missing file: ${result.link}`);
      if (url.hash === '') return;
      if (getToc(target).includes(url.hash)) return;
      throw Error(`Dead: ${result.link}`);
    }

    // A link to a section within this file.
    if (result.link.match(/^#/)) {
      if (toc.includes(result.link)) return;
      throw Error(`Dead: ${result.link}`);
    }

    // A link to an external website.
    switch (result.status) {
      case 'dead':
        if (result.statusCode === 500) {
          console.log(colors.yellow(`Server error on target: ${result.link}`));
          return;
        }
        throw Error(`Dead: ${result.link}`);
      case 'error':
        throw Error(`Error: ${result.link}`);
    }
  }
}

function runLinkCheck(file, {rateLimit}) {
  return new Promise((resolve, reject) => {
    setTimeout(check, rateLimit);

    function check() {
      markdownLinkCheck(
        fs.readFileSync(file).toString(),
        {
          baseUrl: '',
          // If Github rate limit is reached, wait 60s and try again.
          retryOn429: true,
        },
        (error, results) => {
          if (error) {
            reject(error);
            return;
          }
          try {
            verifyLinkCheckResults(file, results);
            resolve();
          } catch (error) {
            reject(error);
          }
        }
      );
    }
  });
}

(async () => {
  for (const file of files) {
    console.log('Checking links in ' + file);
    try {
      await runLinkCheck(file, {rateLimit: 500});
    } catch (error) {
      console.log(colors.red(error.message));
      process.exitCode = 1;
    }
  }
})();
