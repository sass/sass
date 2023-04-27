// The inverse of `tangle.ts`, which takes the generated TypeScript and injects
// it back into the Markdown document. This allow us to format the TypeScript
// files using standard tools while keeping their canonical sources in Markdown.

import * as colors from 'colors/safe';
import * as crypto from 'crypto';
import dedent from 'ts-dedent';
import * as fs from 'fs';
import * as glob from 'glob';
import {marked} from 'marked';
import * as p from 'path';

const args = process.argv.slice(2);
const files = glob.sync(args.length === 0 ? '**/*.d.ts.md' : args, {
  ignore: ['node_modules/**/*.d.ts.md'],
});

for (const file of files) {
  if (!file.endsWith('.md')) {
    console.error(
      `${colors.red(colors.bold('Error:'))} Can't untangle non-.md file ` +
        `"${file}".`
    );
    process.exitCode = 1;
    continue;
  }

  const tangledFile = file.substring(0, file.length - 3);
  const tangledType = p.extname(tangledFile).substring(1);
  if (tangledType === '') {
    console.error(
      `${colors.yellow(colors.bold('Warning:'))} File "${file}" has no ` +
        "additional extension, so there's nothing to untangle it from."
    );
    continue;
  }

  if (!fs.existsSync(tangledFile)) {
    console.error(
      `${colors.red(colors.bold('Error:'))} Can't untangle "${file}" ` +
        `because "${tangledFile}" doesn't exist.`
    );
    process.exitCode = 1;
    continue;
  }

  const codeBlocks = fs
    .readFileSync(tangledFile, 'utf8')
    .split(/\n *\/\/ ==<\[tangle boundary\]>==\n/);

  const match = /^\/\/ <\[tangle hash\]> (.*?)\n/.exec(codeBlocks[0]);
  if (!match) {
    console.error(
      `${colors.red(colors.bold('Error:'))} "${tangledFile}" is missing ` +
        'the initial hash.'
    );
    process.exitCode = 1;
    continue;
  }

  const expectedHash = match[1];
  codeBlocks[0] = codeBlocks[0].split('\n').slice(1).join('\n');

  let source = fs.readFileSync(file, 'utf8');
  const hash = crypto.createHash('sha256').update(source).digest('hex');
  if (hash !== expectedHash) {
    console.error(
      `${colors.red(colors.bold('Error:'))} "${file}" was modified after ` +
        `"${tangledFile}" was tangled.`
    );
    process.exitCode = 1;
    continue;
  }

  let failed = false;
  marked.parse(source, {
    walkTokens: token => {
      if (failed) return;
      if (token.type === 'code' && token.lang === tangledType) {
        let codeBlock = codeBlocks.shift();
        if (codeBlock === undefined) {
          console.error(
            `${colors.red(colors.bold('Error:'))} "${tangledFile}" has fewer ` +
              `code blocks than "${file}".`
          );
          process.exitCode = 1;
          failed = true;
          codeBlock = '';
        }

        source = source.replace(
          token.raw,
          // Add a leading newline to satisfy dedent().
          `\`\`\`${token.lang}\n${dedent('\n' + codeBlock)}\n\`\`\``
        );
      }
    },
  });
  if (failed) continue;

  if (!source.endsWith('\n')) source += '\n';
  fs.writeFileSync(file, source);
  console.log(colors.grey(`Untangled ${tangledFile} to ${file}`));
}
