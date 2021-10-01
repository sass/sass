import {SourceSpan} from './logger';

export class Exception extends Error {
  message: string;

  sassMessage: string;

  sassStack: string;

  span: SourceSpan;

  toString(): string;
}
