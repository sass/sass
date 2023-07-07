import {Value} from './index';

/**
 * Sass's [mixin type](https://sass-lang.com/documentation/values/mixins).
 *
 * @category Custom Function
 */
export class SassMixin extends Value {
  /**
   * Creates a new first-class mixin that can be invoked using
   * [`meta.apply()`](https://sass-lang.com/documentation/modules/meta#apply).
   *
   * @param signature - The function signature, like you'd write for the
   * [`@function rule`](https://sass-lang.com/documentation/at-rules/function).
   * @param callback - The callback that's invoked when this function is called,
   * just like for a {@link CustomFunction}.
   */
  constructor(signature: string, callback: (args: Value[]) => string);
}
