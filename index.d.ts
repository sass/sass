// TypeScript Version: 4.2

export type ImporterReturnType = { file: string } | { contents: string } | Error | null;

export type Importer = (url: string, prev: string, done: (data: ImporterReturnType) => void) => ImporterReturnType | void;

export interface LogData {
    /**
     * The message as-is in the file/data
     */
    message: string;

    /**
     * A set of information related to the location of the error
     */
    source: SourceSpan;

    /**
     * Is it a deprecation message
     */
     deprecation: boolean;
}

export interface Logger {
    /**
     * A function that is called each time `@warn` is encountered
     * @param data All data relative to the `@warn` message
     */
    warn?(data: LogData): void;

    /**
     * A function that is called each time `@debug` is encountered
     * @param data All data relative to the `@debug` message
     */
    debug?(data: LogData): void;
}

export interface Options {
    /**
     * Path to a file to compile.
     *
     * @default null
     */
    file?: string;

    /**
     * A string to pass to compile.
     *
     * It is recommended that you use `includePaths` in conjunction with this so that sass can find files when using the @import directive.
     *
     * @default null
     */
    data?: string;

    /**
     * Handles when the @import directive is encountered.
     *
     * A custom importer allows extension of the sass engine in both a synchronous and asynchronous manner.
     *
     * @default undefined
     */
    importer?: Importer | Importer[];

    /**
     * Holds a collection of custom functions that may be invoked by the sass files being compiled.
     *
     * @default undefined
     */
    functions?: { [key: string]: (...args: types.SassType[]) => types.SassType | void };

    /**
     * An array of paths that should be looked in to attempt to resolve your @import declarations.
     * When using `data`, it is recommended that you use this.
     *
     * @default []
     */
    includePaths?: string[];

    /**
     * Enable Sass Indented Syntax for parsing the data string or file.
     *
     * @default false
     */
    indentedSyntax?: boolean;

    /**
     * Used to determine whether to use space or tab character for indentation.
     *
     * @default 'space'
     */
    indentType?: 'space' | 'tab';

    /**
     * Used to determine the number of spaces or tabs to be used for indentation.
     *
     * @default 2
     */
    indentWidth?: number;

    /**
     * Used to determine which sequence to use for line breaks.
     *
     * @default 'lf'
     */
    linefeed?: 'cr' | 'crlf' | 'lf' | 'lfcr';

    /**
     * Disable the inclusion of source map information in the output file.
     *
     * @default false
     */
    omitSourceMapUrl?: boolean;

    /**
     * Specify the intended location of the output file.
     * Strongly recommended when outputting source maps so that they can properly refer back to their intended files.
     *
     * @default null
     */
    outFile?: string;

    /**
     * Determines the output format of the final CSS style.
     *
     * @default 'expanded'
     */
    outputStyle?: 'compressed' | 'expanded';

    /**
     * Enables the outputting of a source map.
     *
     * @default undefined
     */
    sourceMap?: boolean | string;

    /**
     * Includes the contents in the source map information.
     *
     * @default false
     */
    sourceMapContents?: boolean;

    /**
     * Embeds the source map as a data URI.
     *
     * @default false
     */
    sourceMapEmbed?: boolean;

    /**
     * The value will be emitted as `sourceRoot` in the source map information.
     *
     * @default undefined
     */
    sourceMapRoot?: string;

    /**
     * Callbacks for handling any `@warn` or `@debug` messages
     * 
     * @default undefined
     */
    logger?: Logger;
}

export interface SassException extends Error {
    /**
     * The error message.
     */
    message: string;

    /**
     * The formatted error.
     */
    formatted: string;

    /**
     * The line number of error.
     */
    line: number;

    /**
     * The column number of error.
     */
    column: number;

    /**
     * The status code.
     */
    status: number;

    /**
     * The filename of error.
     *
     * In case file option was not set (in favour of `data`), this will reflect the value `stdin`.
     */
    file: string;
}

export interface SourceLocation {
    /**
     * The 0-indexed offset of the location 
     */
    offset: number;
    
    /**
     * The line number of the location
     */
    line: number;
    
    /**
     * The 0-indexed column of the location 
     */
    column: number;
}

export interface SourceSpan {
    /**
     * The text between `start` and `end` locations.
     * This is from the exact line and column
     */
    text: string;

    /**
     * The start location of the `@warn` or `@debug` message
     */
    start: SourceLocation;

    /**
     * The end location of the `@warn` or `@debug` message
     */
    end: SourceLocation;

    /**
     * The location of the file with the message
     */
    url: string;

    /**
     * The text from `start` to `end`. However, unlike `text`, it outputs each full line
     */
    context: string;
}

export interface Result {
    /**
     * The compiled CSS.
     *
     * Write this to a file, or serve it out as needed.
     */
    css: Buffer;

    /**
     * The source map.
     */
    map?: Buffer;
    stats: {
        /**
         * The path to the scss file, or `data` if the source was not a file.
         */
        entry: string;

        /**
         * `Date.now()` before the compilation.
         */
        start: number;

        /**
         * `Date.now()` after the compilation.
         */
        end: number;

        /**
         * `end - start`
         */
        duration: number;

        /**
         * Absolute paths to all related files in no particular order.
         */
        includedFiles: string[];
    };
}

export function render(options: Options, callback: (exception: SassException, result: Result) => void): void;
export function renderSync(options: Options): Result;

export namespace types {
    abstract class SassType {}

    interface Null extends SassType {
        NULL: Null;
    }

    const Null: Null;

    class Number implements SassType {
        constructor(value: number, unit?: string);
        getValue(): number;
        setValue(value: number): void;
        getUnit(): string;
        setUnit(unit: string): void;
    }

    class String implements SassType {
        constructor(value: string);
        getValue(): string;
        setValue(value: string): void;
    }

    class Boolean<T extends boolean = boolean> implements SassType {
        constructor(value: T);
        getValue(): T;
        static readonly TRUE: Boolean<true>;
        static readonly FALSE: Boolean<false>;
    }

    class HslColor implements SassType {
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

    class RgbColor implements SassType {
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

    class List<T extends SassType = SassType> implements SassType {
        constructor(length: number, commaSeparator?: boolean);
        getValue(index: number): T | undefined;
        setValue(index: number, value: T): void;
        getSeparator(): boolean;
        setSeparator(isComma: boolean): void;
        getLength(): number;
    }

    class Map<K extends SassType = SassType, V extends SassType = SassType> implements SassType {
        constructor(length: number);
        getValue(index: number): V;
        setValue(index: number, value: V): void;
        getKey(index: number): K;
        setKey(index: number, key: K): void;
        getLength(): number;
    }
}
