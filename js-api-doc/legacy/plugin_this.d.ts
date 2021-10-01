export interface LegacyPluginThis {
  options: {
    file?: string;

    data?: string;

    includePaths: string;

    precision: 10;

    style: number;

    indentType: 1 | 0;

    indentWidth: number;

    linefeed: 'cr' | 'crlf' | 'lf' | 'lfcr';

    result: {
      stats: {
        start: number;

        entry: string;
      };
    };
  };
}
