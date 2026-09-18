import {Value} from '.';

/**
 * Sass's [module type](https://sass-lang.com/documentation/values/modules).
 *
 * @category Custom Function
 */
export class SassModule extends Value {
  /**
   * It is not possible to construct a Sass module outside of Sass. Attempting
   * to construct one will throw an exception.
   */
  constructor();
}
