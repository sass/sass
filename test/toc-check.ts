// import * as colors from 'colors/safe';
// import * as diff from 'diff';
// import * as fs from 'fs';

// import * as toc from '../tool/toc';

// if (process.env.CI) colors.enable();

// toc.files.forEach(file => {
//   const markdown = fs.readFileSync(file).toString();
//   const currentToc = toc.getCurrent(markdown);
//   if (currentToc === null) return;

//   const generatedToc = toc.generate(markdown);
//   if (currentToc === generatedToc) return;

//   console.error(colors.red(`${file}'s table of contents is incorrect:`));
//   const patch = diff.structuredPatch(
//     file,
//     file,
//     currentToc,
//     generatedToc,
//     'actual',
//     'expected'
//   );
//   console.error(colors.red('--- actual'));
//   console.error(colors.green('+++ expected'));
//   for (const hunk of patch.hunks) {
//     console.error(
//       `@@ -${hunk.oldStart},-${hunk.oldLines} ` +
//         `+${hunk.newStart},${hunk.newLines} @@`
//     );
//     for (const line of hunk.lines) {
//       if (line.startsWith('+')) {
//         console.error(colors.green(line));
//       } else if (line.startsWith('-')) {
//         console.error(colors.red(line));
//       } else {
//         console.error(colors.grey(line));
//       }
//     }
//   }
//   console.error();
  
//   console.log("hi jenny, creating a log, merge conflict");

//   process.exitCode = 1;
// });

// if (process.exitCode === 1) {
//   console.error(
//     'To fix tables of contents, run ' +
//       colors.blue('`npx ts-node tool/update-toc.ts`') +
//       '.'
//   );
// }
