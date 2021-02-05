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
| file? | string | null | The path to the file to compile (this or `data` is required) |
| data? | string | null | The data string to compile (this or `file` is required) |
| importer? | [Importer][Importer] ***OR*** [Importer][Importer][] | undefined  | A custom extension of the sass engine (See [Importer][Importer] class for more info)  |
| functions? | [A Custom Object][Custom Object] | undefined  | A custom extension of the sass engine (See [Custom Object][Custom Object] for more info and an example)  |
| includePaths? | string[] | [] | An array of paths to resolve. *Recommended when using `data`.* |
| indentedSyntax? | boolean | false | Enable Sass indented syntax for parsing |
| indentType? | `"space"` ***OR*** `"tab"` | `"space"` | To determine which indentation type to use |
| indentWidth? | number | 2 | The number of spaces or tabs to indent by |
| linefeed? | `"cr"` ***OR*** `"crlf"` ***OR*** `"lf"` ***OR*** `"lfcr"` | `"lf"` | To determine which line break to use |
| omitSourceMapUrl? | boolean | false | Wheather or not to include the source map information in the output file |
| outFile? | string | null | Specify the intended output. *Does not save the file!* |
| outputStyle? | `"compressed"` ***OR*** `"expanded"` | `"compressed"` | To determine the output file |
| sourceMap? | boolean ***OR*** string | undefined | `true` will treat the path of source map the same as outFile. A string value will be treated as the intended output path. *Does not output a file*  |
| sourceMapContents? | boolean | false | Include the css contents in the sourcemap data |
| sourceMapEmbed? | boolean | false | Embeds the source map into the css file as a data URI |
| sourceMapRoot? | string | undefined | The `sourceRoot` to be emitted in the source map data |
| logger? | [Logger][Logger] | undefined | Contains callbacks for handling any `@warn` or `@debug` messages |

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
The `functions` property in [RenderOptions][RenderOptions] uses a custom object containing a set of functions, each with a named key. When the `key` is found the accosiated function will be called.

>:warning: The function may take zero or more arguments and must return a value either synchronously (`return ...;`) or asynchronously (`done();`)
>
>The return value must be of one of the same types that has been passed to it (or [SassType.Null][SassType.Null])

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
The logger object has 2 properties: `warn` and `debug`. These functions will be called whenever a `@warn` or `@debug` line is met.

> :information_source: The javascript sass bundle is shipped with a `StdLogger` class. This class will output both `@warn` and `@debug` messages to the `stderr` console.

**Methods**
| Property | Argument(s) | Description |
| --- | --- | --- |
| warn  | [LogData][LogData] | An output function called to handle any `@warn` messages in the file/data |
| debug? | [LogData][LogData] | An output function called to handle any `@debug` messages in the file/data |

### LogData
The related log data that is passed into any `warn` or `debug` method calls of the [Logger][Logger]

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| message  | string | The message **as-is** in the file/data |
| source | [SourceSpan][SourceSpan] | A set of information related to the location of the error |

### SourceSpan
A set of information related to a `@warn` or `@debug` output. This data is included in `source` in the warn and debug methods of [Logger][Logger]

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| text | string | The text between `start` and `end` locations. This is from the exact line **and column** |
| start | [SourceLocation][SourceLocation] | The start location of the warn/debug message that was encountered |
| end? | [SourceLocation][SourceLocation] | The end location of the warn/debug message that was encountered |
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

## Sass Types
>:warning: All these sass types refer to the [RenderOptions][RenderOptions] `function` callback. The type can be an argument of the callback or returned by the callback (apart from null which is never an argument)
>
>e.g. `"Example $info": function(info: SassType.String){ /*use info*/ return SassType.Null.NULL; }`

### SassType.Null
>:information_source: This will never be provided as an argument to a callback. It should only ever be returned by your callback function (synchronously or asynchronously).

**An instance**

You can only create a singleton instance of SassType.Null. This is done with `sassType.Null.NULL`

### SassType.String

**An instance**
| Property | Type(s) | Description |
| --- | --- | --- |
| value | string | The string contents of the type |

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| text | string | The contents of the string |
| quoted | bool | Whether the string is quoted or unquoted |

**Methods**
| Property | Argument(s) | Description |
| --- | --- | --- |
| getValue  | - | Gets the value of the string |
| setValue | string | Sets the value of the string |

### SassType.Number

**An instance**
| Property | Type(s) | Description |
| --- | --- | --- |
| value | number | The numeric contents of the type |
| unit? | string | The contents of the string, you can opt to quote or unquote it |

**Properties**
| Property | Type(s) | Description |
| --- | --- | --- |
| value | number | The numbers numeric value |
| numerators | bool | The number's numerator units |
| denominators | bool | The number's denominator units |

**Methods**
| Property | Argument(s) | Description |
| --- | --- | --- |
| getValue  | - | Gets the value of the number |
| setValue | number | Sets the value of the number |
| getUnit  | - | Gets the value of the unit |
| setUnit | string | Sets the value of the unit |

### SassType.RgbColor

**An instance** (this has two options, see next instance below)
| Property | Type(s) | Description |
| --- | --- | --- |
| r | number | The numeric contents of the Red byte (0 - 255) |
| g | number | The numeric contents of the Green byte (0 - 255) |
| b | number | The numeric contents of the Blue byte (0 - 255) |
| a? | number | The alpha channel of the colour (0.## - 1) |

**An instance** (this has two options, see previous instance above)
| Property | Type(s) | Description |
| --- | --- | --- |
| value | number | The numeric contents of the colour. Specifically a hexidecimal number in the form `rgba` (e.g. `0xff07ac8d`) |

**Methods**
| Property | Argument(s) | Description |
| --- | --- | --- |
| getR  | *returns: number* | Gets the value of the Red byte (0 - 255) |
| setR | number | Sets the value of the Red byte (0 - 255) |
| getG  | *returns: number* | Gets the value of the Green byte (0 - 255) |
| setG | number | Sets the value of the Green byte (0 - 255) |
| getB  | *returns: number* | Gets the value of the Blue byte (0 - 255) |
| setB | number | Sets the value of the Blue byte (0 - 255) |
| getA  | *returns: number* | Gets the value of the alpha channel (0.# - 1) |
| setA | number | Sets the value of the alpha channel (0.# - 1) |

### SassType.HslColor

**An instance**
| Property | Type(s) | Description |
| --- | --- | --- |
| h | number | The numeric contents of the Hue (0 - 100) |
| s | number | The numeric contents of the Saturation (0 - 100) |
| l | number | The numeric contents of the Lightness (0 - 100) |
| a? | number | The alpha channel of the colour (0.## - 1) |

**Methods**
| Property | Argument(s) | Description |
| --- | --- | --- |
| getH  | *returns: number* | Gets the value of the Hue (0 - 100) |
| setH | number | Sets the value of the Hue (0 - 100) |
| getS  | *returns: number* | Gets the value of the Saturation (0 - 100) |
| setS | number | Sets the value of the Saturation (0 - 100) |
| getL  | *returns: number* | Gets the value of the Lightness (0 - 100) |
| setL | number | Sets the value of the Lightness (0 - 100) |
| getA  | *returns: number* | Gets the value of the alpha channel (0.# - 1) |
| setA | number | Sets the value of the alpha channel (0.# - 1) |


<!--

  // A SassScript list value.
  message List {
    // Different types of separators a list can have.
    enum Separator {
      // List elements are separated by a comma.
      COMMA = 0;

      // List elements are separated by whitespace.
      SPACE = 1;

      // List elements are separated by a forward slash.
      SLASH = 2;

      // The list's separator hasn't yet been determined.
      //
      // Singleton lists and empty lists don't have separators defiend. This
      // means that list functions will prefer other lists' separators if
      // possible.
      UNDECIDED = 3;
    }

    // The type of separator for this list. Mandatory.
    Separator separator = 1;

    // Whether this list has square brackets. Mandatory.
    bool has_brackets = 2;

    // The elements of this list.
    repeated Value contents = 3;
  }

  // A SassScript map value.
  message Map {
    // A single key/value pair in the map.
    message Entry {
      // The key this entry is associated with. Mandatory.
      Value key = 1;

      // The value associated with this key. Mandatory.
      Value value = 2;
    }

    // The entries in this map. The sending endpoint must guarantee that no two
    // entries have the same key.
    repeated Entry entries = 1;
  }

  // Singleton SassScript values that have no internal state.
  enum Singleton {
    // The SassScript boolean true value.
    TRUE = 0;

    // The SassScript boolean false value.
    FALSE = 1;

    // The SassScript null value.
    NULL = 2;
  }
-->

<!-- Functions -->
[render]: <#render>
[renderSync]: <#renderSync>

<!-- Classes -->
[RenderOptions]: <#RenderOptions>
[Result]: <#Result>
[Importer]: <#Importer>
[Custom Object]: <#Custom-Object>
[Logger]: <#Logger>
[LogData]: <#LogData>
[SourceSpan]: <#SourceSpan>
[SourceLocation]: <#SourceLocation>
[SassException]: <#SassException>

<!-- Types -->
[SassType]: <#Sass-Types>
[SassType.Null]: <#SassTypeNull>
[SassType.String]: <#SassTypeString>
[SassType.Number]: <#SassTypeNumber>
[SassType.RgbColor]: <#SassTypeRgbColor>
[SassType.HslColor]: <#SassTypeHslColor>
[SassType.List]: <#SassTypeList>
[SassType.Map]: <#SassTypeMap>

<!-- External Links -->
[Fibers]: https://www.npmjs.com/package/fibers
