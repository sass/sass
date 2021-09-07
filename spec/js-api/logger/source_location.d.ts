/** An interface that represents a location in a text file. */
export interface SourceLocation {
  /**
   * The 0-based offset of this location within the file it refers to, in terms
   * of UTF-16 code units.
   */
  offset: number;

  /**
   * The number of U+000A LINE FEED characters between the beginning of the file
   * and `offset`, exclusive.
   *
   * > In other words, this location's 0-based line.
   */
  line: number;

  /**
   * The number of UTF-16 code points between the last U+000A LINE FEED
   * character before `offset` and `offset`, exclusive.
   *
   * > In other words, this location's 0-based column.
   */
  column: number;
}
