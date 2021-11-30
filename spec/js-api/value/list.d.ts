import {List} from 'immutable';

import {Value} from './index';

/**
 * The JS API representation of a Sass list separator.
 *
 * > `null` represents the undecided separator type.
 */
export type ListSeparator = ',' | '/' | ' ' | null;

/**
 * The JS API representation of a Sass list.
 *
 * `internal` refers to a Sass list.
 */
export class SassList extends Value {
  /**
   * Creates a Sass list:
   *
   * - If the first argument is an `Array` or a `List`:
   *   - Let `contents` be the first argument.
   *   - Let `options` be the second argument, or `{}` if it's undefined.
   *
   * - Otherwise:
   *   - Let `contents` be `[]`.
   *   - Let `options` be the first argument, or `{}` if it's undefined.
   *
   * - Let `separator` be `options.separator`, or `','` if that's undefined.
   *
   * - Let `brackets` be `options.brackets`, or `false` if that's undefined.
   *
   * - Set `internal` to a Sass list with contents set to `contents`, separator
   *   set to `separator`, and brackets set to `brackets`.
   *
   * - Return `this`.
   */
  constructor(
    contents: Value[] | List<Value>,
    options?: {
      separator?: ListSeparator;
      brackets?: boolean;
    }
  );

  constructor(options?: {separator?: ListSeparator; brackets?: boolean});

  /** `internal`'s list separator. */
  get separator(): ListSeparator;
}
