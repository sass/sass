import {URL} from 'url';

import {SourceLocation} from './source_location';

export interface SourceSpan {
  start: SourceLocation;

  end: SourceLocation;

  url?: URL;

  text: string;

  context?: string;
}
