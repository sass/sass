// The literate programming "tangle" tool: takes literate programming files (in
// this case, Markdown files with embedded TypeScript) and converts them to the
// corresponding physical TypeScript files.

import * as colors from 'colors/safe';
import * as crypto from 'crypto';
import * as fs from 'fs';
import * as glob from 'glob';
import {marked} from 'marked';
import * as p from 'path';
import * as prettier from 'prettier';

const args = process.argv.slice(2);
const files = glob.sync(args.length === 0 ? '**/*.d.ts.md' : args, {
  ignore: ['node_modules/**/*.d.ts.md'],
});

(async () => {
  for (const file of files) {
    if (!file.endsWith('.md')) {
      console.error(
        `${colors.red(colors.bold('Error:'))} Can't tangle non-.md file ` +
          `"${file}".`
      );
      process.exitCode = 1;
      continue;
    }

    const outputFile = file.substring(0, file.length - 3);
    const outputType = p.extname(outputFile).substring(1);
    if (outputType === '') {
      console.error(
        `${colors.yellow(colors.bold('Warning:'))} File "${file}" has no ` +
          "additional extension, so there's nothing to tangle it to."
      );
    }

    const source = fs.readFileSync(file, 'utf8');
    const hash = crypto.createHash('sha256').update(source).digest('hex');
    const codeBlocks: string[] = [`// <[tangle hash]> ${hash}`];

    marked.parse(source, {
      walkTokens: token => {
        if (token.type === 'code' && token.lang === outputType) {
          codeBlocks.push(token.text);
          codeBlocks.push('// ==<[tangle boundary]>==');
        }
      },
    });
    codeBlocks.push('');

    const config = (await prettier.resolveConfig(outputFile)) ?? {};
    fs.writeFileSync(
      outputFile,
      prettier.format(codeBlocks.join('\n'), {
        filepath: outputFile,
        ...config,
      })
    );
    console.log(colors.grey(`Tangled ${file} to ${outputFile}`));
  }
})();
