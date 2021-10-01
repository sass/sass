import {SourceSpan} from './source_span';

export {SourceLocation} from './source_location';
export {SourceSpan} from './source_span';

export interface Logger {
  warn?(
    message: string,
    options: {
      deprecation: boolean;
      span?: SourceSpan;
      stack?: string;
    }
  ): void;

  debug?(message: string, options: {span: SourceSpan}): void;
}

export namespace Logger {
  export const silent: Logger;
}
