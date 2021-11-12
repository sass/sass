import {OrderedMap} from 'immutable';

import {Value} from './index';

export class SassMap extends Value {
  constructor(contents?: OrderedMap<Value, Value>);

  get contents(): OrderedMap<Value, Value>;

  tryMap(): OrderedMap<Value, Value>;
}
