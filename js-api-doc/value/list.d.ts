import {List} from 'immutable';

import {Value} from './index';

export type ListSeparator = ',' | '/' | ' ' | null;

export class SassList extends Value {
  constructor(
    contents: Value[] | List<Value>,
    options?: {
      /** @default ',' */
      separator?: ListSeparator;
      brackets?: boolean;
    }
  );

  constructor(options?: {separator?: ListSeparator; brackets?: boolean});

  get separator(): ListSeparator;
}
