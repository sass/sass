import {Value} from './index';

export const sassTrue: SassBoolean;

export const sassFalse: SassBoolean;

export interface SassBoolean extends Value {
  get value(): boolean;
}
