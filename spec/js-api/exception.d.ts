/** The error thrown by the compiler when a Sass compilation fails. */
export interface Exception extends Error {
  /**
   * The compiler supplies this error message to the JS runtime. This should
   * contain the description of the Sass exception as well as the Sass span and
   * stack (if available).
   *
   * This message must be passed directly to the super constructor.
   *
   * > The format can vary from implementation to implementation.
   */
  message: string;

  /**
   * The Sass error message, excluding the span and stack.
   *
   * > The format can vary from implementation to implementation.
   */
  sassMessage: string;

  /**
   * The Sass stack trace at the point the error was thrown.
   *
   * > The format can vary from implementation to implementation.
   */
  sassStack?: string;

  /**
   * Provides a formatted string with useful information about the error.
   *
   * > This likely includes the Sass error message, span, and stack. The format
   * > can vary from implementation to implementation.
   */
  toString(): string; // TODO(awjin): Mark this as `override` once TS 4.3 is released.
}
