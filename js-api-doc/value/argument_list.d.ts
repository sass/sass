import {List, OrderedMap} from 'immutable';

import {Value} from './index';
import {SassList, ListSeparator} from './list';

export class SassArgumentList extends SassList {
  constructor(
    contents: Value[] | List<Value>,
    keywords: Record<string, Value> | OrderedMap<string, Value>,
    /** @default ',' */
    separator?: ListSeparator
  );

  get keywords(): OrderedMap<string, Value>;
}
