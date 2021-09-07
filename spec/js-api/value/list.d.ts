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
   * - Set `internal` to a Sass list with contents set to `contents`, separator
   *   set to `options.separator`, and brackets set to `options.brackets`.
   * - Return `this`.
   */
  constructor(
    contents: Value[] | List<Value>,
    options?: {
      /** @default ',' */
      separator?: ListSeparator;
      /** @default false */
      brackets?: boolean;
    }
  );

  /**
   * Creates an empty Sass list:
   *
   * - Set `internal` to an empty Sass list with separator set to
   *   `options.separator` and brackets set to `options.brackets`.
   * - Return `this`.
   */
  static empty(options?: {
    /** @default null */
    separator?: ListSeparator;
    /** @default false */
    brackets?: boolean;
  }): SassList;

  /** `internal`'s list separator. */
  get separator(): ListSeparator;
}
