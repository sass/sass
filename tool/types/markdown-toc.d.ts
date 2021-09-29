declare module 'markdown-toc' {
  namespace toc {
    export interface Options {
      filter?: (header: string) => boolean;
      firsth1?: boolean;
      bullets?: string;
    }
  }

  function toc(markdown: string, options?: toc.Options): {content: string};

  export = toc;
}
