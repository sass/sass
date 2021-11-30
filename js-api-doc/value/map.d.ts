import {OrderedMap} from 'immutable';

import {SassList} from './list';
import {Value} from './index';

export class SassMap extends Value {
  constructor(contents?: OrderedMap<Value, Value>);

  get contents(): OrderedMap<Value, Value>;

  get(key: Value): Value | undefined;

  get(index: number): SassList | undefined;

  tryMap(): SassMap;
}
