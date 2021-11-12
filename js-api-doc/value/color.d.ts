import {Value} from './index';

export class SassColor extends Value {
  constructor(options: {
    red: number;
    green: number;
    blue: number;
    alpha?: number;
  });

  constructor(options: {
    hue: number;
    saturation: number;
    lightness: number;
    alpha?: number;
  });

  constructor(options: {
    hue: number;
    whiteness: number;
    blackness: number;
    alpha?: number;
  });

  get red(): number;

  get green(): number;

  get blue(): number;

  get hue(): number;

  get saturation(): number;

  get lightness(): number;

  get whiteness(): number;

  get blackness(): number;

  get alpha(): number;

  change(options: {
    red?: number;
    green?: number;
    blue?: number;
    alpha?: number;
  }): SassColor;

  change(options: {
    hue?: number;
    saturation?: number;
    lightness?: number;
    alpha?: number;
  }): SassColor;

  change(options: {
    hue?: number;
    whiteness?: number;
    blackness?: number;
    alpha?: number;
  }): SassColor;
}
