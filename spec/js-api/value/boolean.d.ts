import {Value} from './index';

/** The JS API representation of the SassScript true singleton. */
export const sassTrue: SassBoolean;

/** The JS API representation of the SassScript false singleton. */
export const sassFalse: SassBoolean;

/** The JS API representation of a Sass boolean. */
export class SassBoolean extends Value {
  private constructor();

  get value(): boolean;
}
