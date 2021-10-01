import {List} from 'immutable';

import {Value} from './index';

export type ListSeparator = ',' | '/' | ' ' | null;

export class SassList extends Value {
  constructor(
    contents: Value[] | List<Value>,
    options?: {
      /** @default ',' */
      separator?: ListSeparator;
      /** @default false */
      brackets?: boolean;
    }
  );

  static empty(options?: {
    /** @default null */
    separator?: ListSeparator;
    /** @default false */
    brackets?: boolean;
  }): SassList;

  get separator(): ListSeparator;
}
