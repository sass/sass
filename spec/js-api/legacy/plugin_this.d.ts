/**
 * The shared interface for the `this` keyword for custom importers and custom
 * functions. The implementation must invoke importers and custom functions with
 * an appropriate `this`.
 */
export interface LegacyPluginThis {
  options: {
    /** The same `LegacyPluginThis` instance that contains this object. */
    context: LegacyPluginThis;

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
    style: 1;

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
     * A value based on the `linefeed` option passed to the `render()` or
     * `renderSync()`:
     *
     * * If `linefeed` is `"cr"`, this must be `"\r"`.
     * * If `linefeed` is `"crlf"`, this must be `"\r\n"`.
     * * If `linefeed` is `"lf"` or `undefined`, this must be `"\n"`.
     * * If `linefeed` is `"lfcr"`, this must be `"\n\r"`.
     */
    linefeed: '\r' | '\r\n' | '\n' | '\n\r';

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
