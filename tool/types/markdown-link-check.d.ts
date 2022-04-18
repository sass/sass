declare module 'markdown-link-check' {
  namespace linkCheck {
    export interface Options {
      baseUrl?: string;
      retryOn429?: boolean;
      ignorePatterns: Array<{pattern: RegExp}>;
    }

    export interface Result {
      link: string;
      status: string;
      statusCode: number;
    }
  }

  function linkCheck(
    markdown: string,
    options: linkCheck.Options,
    callback: (error: unknown, results: linkCheck.Result[]) => void
  ): void;

  export = linkCheck;
}
