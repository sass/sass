import {OrderedMap} from 'immutable';

import {SassList} from './list';
import {Value} from './index';

/**
 * The JS API representation of a Sass map.
 *
 * `internal` refers to a Sass map.
 */
export class SassMap extends Value {
  /**
   * Creates a Sass map:
   *
   * - If `contents` is undefined, set it to an empty `OrderedMap`.
   * - Set `internal` to a Sass map with contents set to `contents`.
   * - Return `this`.
   */
  constructor(contents?: OrderedMap<Value, Value>);

  /**
   * Returns a map containing `internal`'s contents:
   *
   * - Let `result` be an empty `OrderedMap`.
   * - Add each key and value from `internal`'s contents to `result`, in order.
   * - Return `result`.
   */
  get contents(): OrderedMap<Value, Value>;

  /**
   * - If the first argument is a JavaScript number, pass it to
   *   `this.asList.get` and return the result.
   *
   * - Otherwise, pass it to `this.contents.get` and return the result.
   */
  get(key: Value): Value | undefined;

  get(index: number): SassList | undefined;

  tryMap(): SassMap;
}
