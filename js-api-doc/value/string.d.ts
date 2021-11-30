import {Value} from './index';

export class SassString extends Value {
  constructor(
    text: string,
    options?: {
      /** @default true */
      quotes?: boolean;
    }
  );

  constructor(options?: {quotes?: boolean});

  get text(): string;

  get hasQuotes(): boolean;

  get sassLength(): number;

  sassIndexToStringIndex(sassIndex: Value, name?: string): number;
}
