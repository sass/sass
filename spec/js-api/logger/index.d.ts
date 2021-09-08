import {SourceSpan} from './source_span';

export {SourceLocation} from './source_location';
export {SourceSpan} from './source_span';

/**
 * An object that provides callbacks for handling messages from the
 * compiler.
 */
export interface Logger {
  /**
   * If this field is defined, the compiler must invoke it under the following
   * circumstances:
   *
   * * When it encounters a `@warn` rule:
   *   * Let `value` be the result of evaluating the rule's expression.
   *   * Let `message` be `value`'s text if it's a string, or the result of
   *     serializing `value` if it's not.
   *   * Invoke `warn` with `message` and an object with `deprecation` set to
   *     `false` and `stack` set to a string representation of the current Sass
   *     stack trace.
   *
   *     > The specific format of the stack trace may vary from implementation
   *     > to implementation.
   *
   * * When it encounters anything else that the user needs to be warned about:
   *
   *   > This is intentionally vague about what counts as a warning.
   *   > Implementations have a considerable degree of flexibility in defining
   *   > this for themselves, although in some cases warnings are mandated by
   *   > the specification (such as in preparation for a breaking change).
   *
   *   * Let `options` be an empty object.
   *   * If this warning is caused by behavior that used to be allowed but will
   *     be disallowed in the future, set `options.deprecation` to `true`.
   *     Otherwise, set `options.deprecation` to `false`.
   *   * If this warning is associated with a specific span of a Sass
   *     stylesheet, set `options.span` to a `SourceSpan` that covers that span.
   *   * If this warning occurred during execution of a stylesheet, set
   *     `options.stack` to a string representation of the current Sass stack
   *     trace.
   *   * Invoke `warn` with a string describing the warning and `options`.
   *
   * If this field is defined, the compiler must not surface warnings in any way
   * other than inkoving `warn`.
   */
  warn?(
    message: string,
    options: {
      deprecation: boolean;
      span?: SourceSpan;
      stack?: string;
    }
  ): void;

  /**
   * If this field is defined, the compiler must invoke it when it encounters a
   * `@debug` rule using the following procedure:
   *
   * * Let `value` be the result of evaluating the rule's expression.
   * * Let `message` be `value`'s text if it's a string, or the result of
   *   serializing `value` if it's not.
   * * Invoke `debug` with `message` and an object with `span` set to the span
   *   covering the `@debug` rule and its expression.
   *
   * If this field is defined, the compiler must not surface debug messages in
   * any way other than invoking `debug`.
   */
  debug?(message: string, options: {span: SourceSpan}): void;
}

export namespace Logger {
  /** A Logger that does nothing when it warn or debug methods are called. */
  export const silent: Logger;
}
