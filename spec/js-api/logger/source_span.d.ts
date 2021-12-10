import {SourceLocation} from './source_location';

/**
 * An interface that represents a contiguous section ("span") of a text file.
 * This section may be empty if the `start` and `end` are the same location,
 * in which case it indicates a single position in the file.
 */
export interface SourceSpan {
  /**
   * The location of the first character of this span, unless `end` points to
   * the same character, in which case the span is empty and refers to the point
   * between this character and the one before it.
   */
  start: SourceLocation;

  /**
   * The location of the first character after this span. This must point to a
   * location after `start`.
   */
  end: SourceLocation;

  /**
   * The canonical URL of the file that this span refers to. For files on disk,
   * this must be a `file://` URL.
   *
   * This must be `undefined` for files that are passed to the compiler without
   * a URL. It must not be `undefined` for any files that are importable.
   */
  url?: URL;

  /**
   * The text covered by the span. This must be the text between `start.offset`
   * (inclusive) and `end.offset` (exclusive) of the file referred by this
   * span. Its length must be `end.offset - start.offset`.
   */
  text: string;

  /**
   * Additional source text surrounding this span.
   *
   * The compiler may choose to omit this. If it's not `undefined`, it must
   * contain `text`. Furthermore, `text` must begin at column `start.column` of
   * a line in `context`.
   *
   * > This usually contains the full lines the span begins and ends on if the
   * > span itself doesn't cover the full lines, but the specific scope is up to
   * > the compiler.
   */
  context?: string;
}
