# Javascript API

## Functions

### render
An asyncronous function that requires a set of options to render a sass file or path to be returned into a callback function

> :warning: [renderSync][renderSync] is more than twice as fast due to async overheads. This can be reduced by using the [`fibers`][Fibers] package

**Arguments**
| Argument | Type(s) | Description |
| --- | --- | --- |
| options | [RenderOptions][RenderOptions] | An object containing all the options for handling the file |
| callback | callback: ([SassException][SassException], [Result][Result]) | A callback containing the result of the compilation process or any errors |

### renderSync
A syncronous function that requires a set of options to render a sass file or path to return a result object.  
**Throws:** [SassException][SassException] on error

**Arguments**
| Argument | Type(s) | Description |
| --- | --- | --- |
| options | [RenderOptions] | An object containing all the options for handling the file |

**Ouput**

| Type(s) | Description |
| --- | --- |
| [Result][Result]  | An object containing the result of the compilation process |

## Classes

### RenderOptions
A set of options that must be passed into the [render][render] or [renderSync][renderSync] functions

> :warning: either `file` or `data` is required, excluding both will result in an error. If both are included, `file` takes precedence over `data`

> :warning: `outFile` and `sourceMap` do not ouput a file! `outFile` is strongly recommended when outputting source map files!

**Properties**
| Property | Type(s) | Default | Description |
| --- | --- | --- | --- |
| file | string | null | Optional: The path to the file to compile (this or `data` is required) |
| data | string | null | Optional: The data string to compile (this or `file` is required) |
| importer | [Importer][Importer] ***OR*** [Importer][Importer][] | undefined  | Optional: A custom extension of the sass engine (See [Importer][Importer] class for more info)  |
| functions | [A Custom Object][Custom Object] | undefined  | A custom extension of the sass engine (See [Custom Object][Custom Object] for more info and an example)  |
| includePaths | string[] | [] | Optional: An array of paths to resolve. *Recommended when using `data`.* |
| indentedSyntax | boolean | false | Optional: Enable Sass indented syntax for parsing |
| indentType | `"space"` ***OR*** `"tab"` | `"space"` | Optional: To determine which indentation type to use |
| indentWidth | number | 2 | Optional: The number of spaces or tabs to indent by |
| linefeed | `"cr"` ***OR*** `"crlf"` ***OR*** `"lf"` ***OR*** `"lfcr"` | `"lf"` | Optional: To determine which line break to use |
| omitSourceMapUrl | boolean | false | Optional: Wheather or not to include the source map information in the output file |
| outFile | string | null | Optional: Specify the intended output. *Does not save the file!* |
| outputStyle | `"compressed"` ***OR*** `"expanded"` | `"compressed"` | Optional: To determine the output file |
| sourceMap | boolean ***OR*** string | undefined | Optional: `true` will treat the path of source map the same as outFile. A string value will be treated as the intended output path. *Does not output a file*  |
| sourceMapContents | boolean | false | Optional: Include the css contents in the sourcemap data |
| sourceMapEmbed | boolean | false | Optional: Embeds the source map into the css file as a data URI |
| sourceMapRoot | string | undefined | Optional: The `sourceRoot` to be emitted in the source map data |
| logger | [Logger][Logger] | undefined | Optional: Contains callbacks for handling any `@warn` or `@debug` messages |

### Importer
A function, or set of functions, that's called when sass encounters an `@import` statement. The function must either `return` (for syncronous code) or call `done` (for asyncronous code) with an object containing either a `file` or `contents` property. 

> :warning:  
> When returning or calling `done()` with `{ file: "string" }`, the new file path will be assumed for the `@import`. It's recommended to be mindful of the value of `prev` in instances where relative path resolution may be required.  
> When returning or calling `done()` with `{ contents: "string" }`, the string value will be used as if the file was read in through an external source.  
> You can also return or call `done()` with an `Error`, when appropriate.

**Arguments**
| Argument | Type(s) | Description |
| --- | --- | --- |
| url | string | The import path **as-is** in the file. *It is not resolved!* |
| prev | string | The previously resolved path |
| done | callback | A callback function to invoke on async completion |

### Custom Object
The `functions` property in [RenderOptions][RenderOptions] uses a custom object containing a set functions with named keys. When the `key` is found the accosiated function will be called.

**Basic Layout**
```JS
functions: { 
    "headings($from: 0, $to: 6)": function(from, to) {
        var i, f = from.getValue(), t = to.getValue(),
            list = new sass.types.List(t - f + 1);

        for (i = f; i <= t; i++) {
            list.setValue(i - f, new sass.types.String('h' + i));
        }
        
        return list;
    }
}
```

**Argument set**
| Argument | Type(s) | Description |
| --- | --- | --- |
| key | string | The key that may be found in the file being processed |
| function | function(...args [SassType][SassType][]) | The function to be called when the key is found |

### Logger
The logger object has 2 properties: `warn` and `debug`. These functions will be called henever a `@warn` or `@debug` line is met.

> :information_source: The javascript sass bundle is shipped with a `StdLogger` class. This class will output both `@warn` and `@debug` messages to the `stderr` console.

**Properties (both properties must have the arguments shown in the Arguments table below)**
| Property | Type(s) | Description |
| --- | --- | --- |
| warn  | Function | An output function called to handle any `@warn` messages in the file/data |
| debug | Function | An output function called to handle any `@debug` messages in the file/data |

**Arguments**
| Argument | Type(s) | Description |
| --- | --- | --- |
| message  | string | The message **as-is** in the file/data |
| source | [SourceSpan][SourceSpan] | A set of information related to the location of the error |

### SourceSpan
A set of information related to a `@warn` or `@debug` output. This data is included in `source` in the warn and debug properties of [Logger][Logger]

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| text | string | The text between `start` and `end` locations. This is from the exact line **and column** |
| start | [SourceLocation][SourceLocation] | The start location of the warn/debug message that was encountered |
| end | [SourceLocation][SourceLocation] | The end location of the warn/debug message that was encountered |
| url | string | The url of the file that rasing the warn/debug message |
| context | string | The text from `start` to `end`. However, unlike `text`, it includes the full lines |

### SourceLocation
The location of the associated `start` or `end` location of the [SourceSpan][SourceSpan]

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| offset | number | The 0-based offset where the issue was raised  |
| line | number | The 0-based line of the file where the issue was raised |
| column | number | The 0-based column of the file where the issue was raised |

### SassException
An error thrown by an unexpected event/code block within the processing file/data.

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| status | number | The status code of the error |
| message | string | The error message that was thrown |
| formatted | string | A user-friendly formatted string, suitable to display in a console of some sort |
| line | number | The 0-based line of the file/data being processed |
| column | number | The 0-based column of the file/data being processed |
| file | string | A path to the file that caused the error to be thrown |

<!-- Functions -->
[render]: <#render>
[renderSync]: <#renderSync>

<!-- Classes -->
[RenderOptions]: <#RenderOptions>
[Result]: <#Result>
[Importer]: <#Importer>
[Custom Object]: <#Custom-Object>
[Logger]: <#Logger>
[SourceSpan]: <#SourceSpan>
[SourceLocation]: <#SourceLocation>
[SassException]: <#SassException>

<!-- Types -->
[SassType]: <#SassType>

<!-- External Links -->
[Fibers]: https://www.npmjs.com/package/fibers
