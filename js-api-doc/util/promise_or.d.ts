/**
 * A utility type for choosing between synchronous and asynchronous return
 * values.
 *
 * This is used as the return value for plugins like [[CustomFunction]],
 * [[Importer]], and [[FileImporter]] so that TypeScript enforces that
 * asynchronous plugins are only passed to [[compileAsync]] and
 * [[compileStringAsync]], not [[compile]] or [[compileString]].
 *
 * @typeParam sync - If this is `'sync'`, this can only be a `T`. If it's
 * `'async'`, this can be either a `T` or a `Promise<T>`.
 *
 * @category Other
 */
export type PromiseOr<T, sync extends 'sync' | 'async'> = sync extends 'async'
  ? T | Promise<T>
  : T;
