import {SourceSpan} from './logger';

/**
 * The error thrown by the compiler when a Sass compilation fails. This should
 * *not* be thrown for errors that occur outside of Sass compilation, such as
 * argument verification errors.
 */
export class Exception extends Error {
  private constructor();

  /**
   * The compiler supplies this error message to the JS runtime. This should
   * contain the description of the Sass exception as well as human-friendly
   * representations of `span` and `sassStack` (if they're set).
   *
   * This message must be passed directly to the super constructor.
   *
   * > The format can vary from implementation to implementation.
   */
  message: string;

  /**
   * The Sass error message, excluding the human-friendly representation of
   * `span` and `sassStack`.
   *
   * > The format can vary from implementation to implementation.
   */
  readonly sassMessage: string;

  /**
   * A human-friendly representation of the loads, function calls, and mixin
   * includes that were active when this error was thrown.
   *
   * > The format can vary from implementation to implementation.
   */
  readonly sassStack: string;

  /**
   * A span whose `url` is the canonical URL of the stylesheet being parsed or
   * evaluated, and whose `start` points to the line in that stylesheet on which
   * the error occurred.
   *
   * > The other details of this span can vary from implementation to
   * > implementation, but implementations are strongly encouraged to ensure
   * > that this covers a span of text that clearly indicates the location of
   * > the error.
   */
  readonly span: SourceSpan;

  /**
   * Provides a formatted string with useful information about the error.
   *
   * > This likely includes the Sass error message, span, and stack. The format
   * > can vary from implementation to implementation.
   */
  toString(): string; // TODO(awjin): Mark this as `override` once TS 4.3 is released.
}
