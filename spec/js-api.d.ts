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
 *
 * ## API
 */

interface FileOptions extends SharedOptions {
  file: string;
}

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

type Importer = (
  this: ImporterThis,
  url: string,
  prev: string
) => {file: string} | {contents: string};

type ImporterAsync = (
  this: ImporterThis,
  url: string,
  prev: string,
  done: (data: {file: string} | {contents: string} | Error) => void
) => void;

interface Options extends SharedOptions {
  importer?: Importer | Importer[];
  functions?: SassFunction;
}

interface OptionsAsync extends SharedOptions {
  fibers?: unknown;
  importer?: ImporterAsync | ImporterAsync[];
  functions?: SassFunctionAsync;
}

interface Result {
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

export function render(
  options: RenderOptionsAsync,
  callback: (exception: SassException, result: Result) => void
): void;

export type RenderOptionsAsync = (FileOptions | StringOptions) & OptionsAsync;

export function renderSync(options: RenderOptions): Result;

export type RenderOptions = (FileOptions | StringOptions) & Options;

interface StringOptions extends SharedOptions {
  data: string;
  file?: string;
}

interface SassFunction {
  [key: string]: (
    this: PluginThis,
    ...args: types.SassType[]
  ) => types.SassType;
}

interface SassFunctionAsync {
  [key: string]: <T extends types.SassType[]>(
    this: PluginThis,
    ...args: [...T, (type: types.SassType) => void]
  ) => void;
}

interface SharedOptions {
  includePaths?: string[];
  indentedSyntax?: boolean;
  indentType?: 'space' | 'tab';
  indentWidth?: number;
  linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';
  omitSourceMapUrl?: boolean;
  outFile?: string;
  outputStyle?: 'compressed' | 'expanded';
  sourceMap?: boolean | string;
  sourceMapContents?: boolean;
  sourceMapEmbed?: boolean;
  sourceMapRoot?: string;
}

interface SassException extends Error {
  message: string;
  formatted: string;
  line: number;
  column: number;
  status: number;
  file: string;
}

export namespace types {
  abstract class SassType {}

  export class Null extends SassType {
    static NULL: Null;
  }

  export class Number implements SassType {
    constructor(value: number, unit?: string);
    getValue(): number;
    setValue(value: number): void;
    getUnit(): string;
    setUnit(unit: string): void;
  }

  export class String implements SassType {
    constructor(value: string);
    getValue(): string;
    setValue(value: string): void;
  }

  export class Boolean<T extends boolean = boolean> implements SassType {
    constructor(value: T);
    getValue(): T;
    static readonly TRUE: Boolean<true>;
    static readonly FALSE: Boolean<false>;
  }

  export class HslColor implements SassType {
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

  export class RgbColor implements SassType {
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

  export class List<T extends SassType = SassType> implements SassType {
    constructor(length: number, commaSeparator?: boolean);
    getValue(index: number): T | undefined;
    setValue(index: number, value: T): void;
    getSeparator(): boolean;
    setSeparator(isComma: boolean): void;
    getLength(): number;
  }

  export class Map<K extends SassType = SassType, V extends SassType = SassType>
    implements SassType
  {
    constructor(length: number);
    getValue(index: number): V;
    setValue(index: number, value: V): void;
    getKey(index: number): K;
    setKey(index: number, key: K): void;
    getLength(): number;
  }
}
