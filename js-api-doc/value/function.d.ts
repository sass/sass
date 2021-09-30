import {Value} from './index';

export class SassFunction extends Value {
  constructor(signature: string, callback: (args: Value[]) => Value);
}
