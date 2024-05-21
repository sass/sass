import * as crypto from 'crypto';
import * as fs from 'fs';

const yamlFile = 'spec/deprecations.yaml';
const specFile = 'spec/js-api/deprecations.d.ts.md';
const docFile = 'js-api-doc/deprecations.d.ts';

(async () => {
  const yamlText = fs.readFileSync(yamlFile, 'utf8');
  const specText = fs.readFileSync(specFile, 'utf8');
  const docText = fs.readFileSync(docFile, 'utf8');

  const checksum = crypto.createHash('sha1').update(yamlText).digest('hex');

  if (
    !specText.includes(`<!-- Checksum: ${checksum} -->`) ||
    !docText.includes(`// Checksum: ${checksum}`)
  ) {
    console.error('Deprecations out-of-sync. Run `npm run sync-deprecations`.');
    process.exitCode = 1;
  }
})();
