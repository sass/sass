import * as colors from 'colors/safe';
import * as diff from 'diff';
import * as fs from 'fs';
import * as glob from 'glob';
import * as p from 'path';
import * as prettier from 'prettier';
import strip = require('strip-comments');

for (const specPath of glob.sync('spec/js-api/**/*.d.ts')) {
  const specFile = prettier.format(strip(fs.readFileSync(specPath, 'utf-8')), {
    filepath: specPath,
  });
  const docPath = p.join('js-api-doc', p.relative('spec/js-api', specPath));

  if (!fs.existsSync(docPath)) {
    process.stderr.write(
      colors.red(`${specPath} exists but ${docPath} does not.\n`)
    );
    process.exitCode = 1;
    continue;
  }

  const docFile = prettier.format(strip(fs.readFileSync(docPath, 'utf-8')), {
    filepath: specPath,
  });
  if (specFile === docFile) continue;

  const diffResult = diff.createTwoFilesPatch(
    specPath,
    docPath,
    specFile,
    docFile
  );
  for (const line of diffResult.split('\n')) {
    if (line.startsWith('-')) {
      process.stderr.write(colors.red(`${line}\n`));
    } else if (line.startsWith('+')) {
      process.stderr.write(colors.green(`${line}\n`));
    } else {
      process.stderr.write(`${line}\n`);
    }
  }
  process.exitCode = 1;
}
