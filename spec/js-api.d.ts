/**
 * # JavaScript API
 *
 * Sass implementations that are available for use via JavaScript must expose
 * the following JavaScript API. As with the rest of this specification, they
 * must not add custom extensions that aren't shared across all implementations.
 *
 * > Having a shared, consistent API makes it easy for users to move between
 * > Sass implementations with minimal disruption, and for build system plugins
 * > to seamlessly work with multiple implementations.
 *
 * The JS API is specified as a TypeScript type declaration. Implementations
 * must adhere to this declaration and to the behavioral specifications written
 * in JSDoc comments on the declarations. Implementations may throw errors when
 * user code passes in values that don't adhere to the type declaration, but
 * they may also handle these values in undefined ways in accordance with the
 * common JavaScript pattern of avoiding explicit type checks. This must not be
 * used as a way of adding custom extensions that aren't shared across all
 * implementations.
 *
 * As with other sections of this specification, the specification of the JS API
 * is incomplete, and is added to *lazily*. This means that portions of the
 * spec—particularly the documentation comments that serve as a behavioral
 * specification—are only written when they're necessary as background for new
 * API proposals.
 */

/** ## API */

/** ### Options */

/**
 * All the options for a Sass compilation except those that specify the specific
 * input format.
 *
 * This is only exported so that it can be modified by proposals for new
 * features. It should not be referred to by user code.
 */
export interface _Options<sync = 'sync' | 'async'> {
  includePaths?: string[];
  indentedSyntax?: boolean;
  indentType?: 'space' | 'tab';
  indentWidth?: number;
  linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';
  omitSourceMapUrl?: boolean;
  outFile?: string;
  outputStyle?: 'compressed' | 'expanded' | 'nested' | 'compact';
  sourceMap?: boolean | string;
  sourceMapContents?: boolean;
  sourceMapEmbed?: boolean;
  sourceMapRoot?: string;
  importer?: Importer<sync> | Importer<sync>[];
  functions?: {[key: string]: CustomFunction<sync>};
}

export type Options<sync = 'sync' | 'async'> = _Options<sync> &
  (
    | {
        file: string;
      }
    | {
        data: string;
        file?: string;
      }
  );

/** #### Shared Plugin Infrastructure */

/**
 * The shared interface for the `this` keyword for custom importers and custom
 * functions. The implementation must invoke importers and custom functions with
 * an appropriate `this`.
 */
interface PluginThis {
  options: {
    /** The `file` option passed to the `render()` or `renderSync()` call. */
    file?: string;

    /** The `data` option passed to the `render()` or `renderSync()` call. */
    data?: string;

    /**
     * A string that contains the current working directory followed by strings
     * passed in the `includePaths` option, separated by `";"` on Windows and
     * `":"` elsewhere.
     */
    includePaths: string;

    precision: 10;

    /**
     * An integer. The specific semantics of this are left up to the
     * implementation. (The reference implementation always returns 1.)
     */
    style: number;

    /**
     * The number 1 if the `indentType` option was `tab`. The number 0
     * otherwise.
     */
    indentType: 1 | 0;

    /**
     * An integer indicating the number of spaces or tabs emitted by the
     * compiler for each level of indentation.
     */
    indentWidth: number;

    /**
     * The `linefeed` option passed to the `render()` or `renderSync()`, or
     * `'lf'` if no value was passed.
     */
    linefeed: 'cr' | 'crlf' | 'lf' | 'lfcr';

    result: {
      stats: {
        /**
         * The number of milliseconds since the Unix epoch (1 January 1970
         * 00:00:00 UT) at the point at which the user called `render()` or
         * `renderSync()`.
         */
        start: number;

        /**
         * The `file` option passed to the `render()` call, or the string
         * `"data"` if no file was passed.
         */
        entry: string;
      };
    };
  };
}

/** #### Importer Plugins */

/**
 * The interface for the `this` keyword for custom importers. The implementation
 * must invoke importers with an appropriate `this`.
 */
interface ImporterThis extends PluginThis {
  /**
   * `true` if this importer invocation was caused by an `@import` statement and
   * `false` otherwise.
   *
   * > This allows importers to look for `.import.scss` stylesheets if and only
   * > if an `@import` is being resolved.
   */
  fromImport: boolean;
}

type _SyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string
) => {file: string} | {contents: string};

type _AsyncImporter = (
  this: ImporterThis,
  url: string,
  prev: string,
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

export type Importer<sync = 'sync' | 'async'> =
  | _SyncImporter
  | (sync extends 'async' ? _AsyncImporter : never);

/** #### Function Plugins */

type _SyncFunction = (this: PluginThis, ...args: Value[]) => Value;

type _AsyncFunction = (
  this: PluginThis,
  ...args: [...Value[], (type: Value) => void]
) => void;

export type CustomFunction<sync = 'sync' | 'async'> =
  | _SyncFunction
  | (sync extends 'async' ? _AsyncFunction : never);

export type Value =
  | types.Null
  | types.Number
  | types.String
  | types.Boolean
  | types.HslColor
  | types.RgbColor
  | types.List
  | types.Map;

export namespace types {
  export class Null {
    static NULL: Null;
  }

  export class Number {
    constructor(value: number, unit?: string);
    getValue(): number;
    setValue(value: number): void;
    getUnit(): string;
    setUnit(unit: string): void;
  }

  export class String {
    constructor(value: string);
    getValue(): string;
    setValue(value: string): void;
  }

  export class Boolean<T extends boolean = boolean> {
    constructor(value: T);
    getValue(): T;
    static readonly TRUE: Boolean<true>;
    static readonly FALSE: Boolean<false>;
  }

  export class HslColor {
    constructor(h: number, s: number, l: number, a?: number);
    getH(): number;
    setH(value: number): void;
    getS(): number;
    setS(value: number): void;
    getL(): number;
    setL(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  export class RgbColor {
    constructor(r: number, g: number, b: number, a?: number);
    getR(): number;
    setR(value: number): void;
    getG(): number;
    setG(value: number): void;
    getB(): number;
    setB(value: number): void;
    getA(): number;
    setA(value: number): void;
  }

  export class List {
    constructor(length: number, commaSeparator?: boolean);
    getValue(index: number): Value | undefined;
    setValue(index: number, value: Value): void;
    getSeparator(): boolean;
    setSeparator(isComma: boolean): void;
    getLength(): number;
  }

  export class Map {
    constructor(length: number);
    getValue(index: number): Value;
    setValue(index: number, value: Value): void;
    getKey(index: number): Value;
    setKey(index: number, key: Value): void;
    getLength(): number;
  }
}

/** ### Render */

export interface Result {
  css: Buffer;
  map?: Buffer;
  stats: {
    entry: string;
    start: number;
    end: number;
    duration: number;
    includedFiles: string[];
  };
}

export function renderSync(options: Options<'sync'>): Result;

export function render(
  options: Options<'async'>,
  callback: (exception: SassException, result: Result) => void
): void;

/** ### Exceptions */

export interface SassException extends Error {
  message: string;
  formatted: string;
  line: number;
  column: number;
  status: number;
  file: string;
}
