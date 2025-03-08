import * as crypto from 'crypto';
import * as fs from 'fs';
import {parse} from 'yaml';

interface YamlData {
  [key: string]: {
    description: string;
    'dart-sass': {
      status: 'active' | 'future' | 'obsolete';
      deprecated?: string;
      obsolete?: string;
    };
  };
}

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
    return;
  }

  const deprecations = parse(yamlText) as YamlData;
  for (const [id, deprecation] of Object.entries(deprecations)) {
    const dartSass = deprecation['dart-sass'];

    if (dartSass.deprecated && dartSass.status === 'future') {
      console.error(
        `Deprecation "${id}" has a version but its status is future.`
      );
      process.exitCode = 1;
    } else if (!dartSass.deprecated && dartSass.status !== 'future') {
      console.error(
        `Deprecation "${id}" has status ${dartSass.status} but no deprecated ` +
          'version.'
      );
      process.exitCode = 1;
    } else if (dartSass.obsolete && dartSass.status !== 'obsolete') {
      console.error(
        `Deprecation "${id}" has an obsolete version but its status isn't ` +
          'obsolete.'
      );
      process.exitCode = 1;
    } else if (!dartSass.obsolete && dartSass.status === 'obsolete') {
      console.error(
        `Deprecation "${id}" has status obsolete but no obsolete version.`
      );
      process.exitCode = 1;
    }
  }
})();
