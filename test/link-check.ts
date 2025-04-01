import * as colors from 'colors/safe';
import * as fs from 'fs';
import * as glob from 'glob';
import markdownLinkCheck = require('markdown-link-check');
import * as linkCheck from 'markdown-link-check';
import markdownToc = require('markdown-toc');
import * as path from 'path';
import {fileURLToPath, pathToFileURL, URL} from 'url';
import * as indentString from 'indent-string';

if (process.env.CI) colors.enable();

const args = process.argv.slice(2);
const files = glob.sync(args.length === 0 ? '**/*.md' : args, {
  ignore: ['node_modules/**/*.md', 'js-api-doc/**/*.md'],
});

const tocCache = new Map<string, string>();

function getToc(file: string): string {
  file = path.normalize(file);
  let toc = tocCache.get(file);
  if (toc === undefined) {
    toc = markdownToc(fs.readFileSync(file).toString(), {}).content;
    tocCache.set(file, toc);
  }
  return toc;
}

function pathIsWithin(child: string, parent: string): boolean {
  const relative = path.relative(parent, child);
  return !relative.startsWith('../');
}

function flagDeadLink(link: string): void {
  console.error(`${colors.red(colors.bold('Dead:'))} ${link}`);
  process.exitCode = 1;
}

function verifyLinkCheckResults(
  file: string,
  results: linkCheck.Result[]
): void {
  for (const result of results) {
    const url = new URL(result.link, pathToFileURL(file));

    // A link to another file.
    if (url.protocol === 'file:' && !result.link.match(/ \(.*\)$/)) {
      const target = fileURLToPath(url);
      if (!fs.existsSync(target)) {
        flagDeadLink(result.link);
      } else if (url.hash !== '' && !getToc(target).includes(`(${url.hash})`)) {
        flagDeadLink(result.link);
      } else if (
        result.link.includes('../spec/') &&
        pathIsWithin(file, 'spec')
      ) {
        console.error(
          `${colors.yellow(colors.bold('Unnecessary ../spec:'))} ${result.link}`
        );
        process.exitCode = 1;
      }
      continue;
    }

    // A link to an external website.
    switch (result.status) {
      case 'dead':
        if (result.statusCode === 500) {
          console.error(
            colors.yellow(`Server error on target: ${result.link}`)
          );
        } else {
          flagDeadLink(result.link);
        }
        break;

      case 'error':
        console.error(
          `${colors.red(colors.bold('Error:'))} ${result.link}` +
            (result.err
              ? '\n' + indentString(result.err.toString(), 1, {indent: '| '})
              : '')
        );
        process.exitCode = 1;
    }
  }
}

function runLinkCheck(
  file: string,
  {rateLimit}: {rateLimit?: number}
): Promise<void> {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      markdownLinkCheck(
        fs.readFileSync(file).toString(),
        {
          baseUrl: pathToFileURL(file).toString(),
          // If Github rate limit is reached, wait 60s and try again.
          retryOn429: true,
          ignorePatterns: [
            // Twitter links consistently fail.
            {pattern: /^https?:\/\/twitter\.com(\/|$)/},
            // tcort/markdown-link-check#260
            {pattern: /^https?:\/\/blogs\.msdn\.microsoft\.com(\/|$)/},
            // Link consistently fails within CI
            {pattern: /^https:\/\/runtime-keys\.proposal\.wintercg\.org(\/|$)/},
            // Stackoverflow links can get rate-limited on GitHub Actions
            {pattern: /^https:\/\/stackoverflow\.com(\/|$)/},
            // W3C links started failing in March 2025
            {pattern: /^https:\/\/(drafts\.)?csswg\.org(\/|$)/},
          ],
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
    }, rateLimit);
  });
}

(async () => {
  for (const file of files) {
    console.log(colors.grey('Checking links in ' + file));
    try {
      await runLinkCheck(file, {rateLimit: 500});
    } catch (error) {
      console.error(error);
      process.exitCode = 1;
    }
  }
})();
