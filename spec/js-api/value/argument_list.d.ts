import {List, OrderedMap} from 'immutable';

import {Value} from './index';
import {SassList, ListSeparator} from './list';

/**
 * The JS API representation of a Sass argument list.
 *
 * `internal` refers to a Sass argument list.
 */
export class SassArgumentList extends SassList {
  /**
   * Creates a Sass argument list:
   *
   * - Set `internal` to a Sass argument list with contents set to `contents`,
   *   keywords set to `keywords`, and list separator set to `separator`.
   * - Return `this`.
   */
  constructor(
    contents: Value[] | List<Value>,
    keywords: Record<string, Value> | OrderedMap<string, Value>,
    /** @default ',' */
    separator?: ListSeparator
  );

  /** `internal`'s keywords. */
  get keywords(): OrderedMap<string, Value>;
}
