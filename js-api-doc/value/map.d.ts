import {OrderedMap} from 'immutable';

import {Value} from './index';

export class SassMap extends Value {
  constructor(contents: OrderedMap<Value, Value>);

  static empty(): SassMap;

  get contents(): OrderedMap<Value, Value>;

  tryMap(): OrderedMap<Value, Value>;
}
