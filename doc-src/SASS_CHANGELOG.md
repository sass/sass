# Sass Changelog

* Table of contents
{:toc}

## 2.4.0 (Unreleased)

### Colors

Sass now supports functions that return the values of the
{Sass::Script::Functions#red red},
{Sass::Script::Functions#blue blue},
and {Sass::Script::Functions#green green}
components of colors.

### Variable Names

SassScript variable names may now contain hyphens.
For example:

    !prettiest-color = #542FA9

### Error Backtraces

Numerous bugs were fixed with the backtraces given for Sass errors,
especially when importing files.
All imports will now show up in the Ruby backtrace,
with the proper filename and line number.

In addition, when the `sass` executable encounters an error,
it now prints the filename where the error occurs,
as well as a backtrace of Sass imports.

### Formatting

Properties of the form

    margin: auto
      top: 10px
      bottom: 20px

That is, properties with a value and *also* nested properties,
are now rendered as such in nested output mode:

    margin: auto;
      margin-top: 10px;
      margin-bottom: 20px;

That is, with the nested properties indented in the source.

### Ruby 1.9 Support

Sass and `css2sass` now produce more descriptive errors
when given a template with invalid byte sequences for that template's encoding,
including the line number and the offending character.

### `css2sass` Error Handling

Several bug fixes and minor improvements have been made, including:

* Fixing line-number reporting for errors on the last line of templates
  that didn't have trailing newlines.

* Only displaying the text for the current line when reporting CSS parsing errors.

* Displaying the expected strings as strings rather than regular expressions
  whenever possible.

### Minor Changes

* If a CSS or Sass function is used that has the name of a color,
  it will now be parsed as a function rather than as a color.
  For example, `fuchsia(12)` now renders as `fuchsia(12)`
  rather than `fuchsia 12`.

## 2.2.14 (Unreleased)

* {Sass::Script::Color#value} attribute is deprecated.
  Use {Sass::Script::Color#rgb} instead.
  This only affects people defining their own Sass functions
  in Ruby.

* All Sass functions now raise explicit errors if their inputs
  are of the incorrect type.

* Add an `assert_type` function that's available to {Sass::Script::Functions}.
  This is useful for typechecking the inputs to functions.

### Rack Support

Sass 2.2.14 includes Rack middleware for running Sass,
meaning that all Rack-enabled frameworks can now use Sass.
To activate this, just add

    require 'sass/plugin/rack'
    use Sass::Plugin::Rack

to your `config.ru`.
See the {Sass::Plugin::Rack} documentation for more details.

## [2.2.13](http://github.com/nex3/haml/commit/2.2.13)

There were no changes made to Sass between versions 2.2.12 and 2.2.13.

## [2.2.12](http://github.com/nex3/haml/commit/2.2.12)

* Fix a stupid bug introduced in 2.2.11 that broke the Sass Rails plugin.

## [2.2.11](http://github.com/nex3/haml/commit/2.2.11)

* Added a note to errors on properties that could be pseudo-classes (e.g. `:focus`)
  indicating that they should be backslash-escaped.

* Automatically interpret properties that could be pseudo-classes as such
  if {file:SASS_REFERENCE.md.html#property_syntax-option `:property_syntax`}
  is set to `:new`.

* Fixed `css2sass`'s generation of pseudo-classes so that they're backslash-escaped.

* Don't crash if the Haml plugin skeleton is installed and `rake gems:install` is run.

* Don't use `RAILS_ROOT` directly.
  This no longer exists in Rails 3.0.
  Instead abstract this out as `Haml::Util.rails_root`.
  This changes makes Haml fully compatible with edge Rails as of this writing.

* Make use of a Rails callback rather than a monkeypatch to check for stylesheet updates
  in Rails 3.0+.

## [2.2.10](http://github.com/nex3/haml/commit/2.2.10)

* Add support for attribute selectors with spaces around the `=`.
  For example:

      a[href = http://google.com]
        color: blue

## [2.2.9](http://github.com/nex3/haml/commit/2.2.9)

There were no changes made to Sass between versions 2.2.8 and 2.2.9.

## [2.2.8](http://github.com/nex3/haml/commit/2.2.8)

There were no changes made to Sass between versions 2.2.7 and 2.2.8.

## [2.2.7](http://github.com/nex3/haml/commit/2.2.7)

There were no changes made to Sass between versions 2.2.6 and 2.2.7.

## [2.2.6](http://github.com/nex3/haml/commit/2.2.6)

* Don't crash when the `__FILE__` constant of a Ruby file is a relative path,
  as apparently happens sometimes in TextMate
  (thanks to [Karl Varga](http://github.com/kjvarga)).

* Add "Sass" to the `--version` string for the executables.

## [2.2.5](http://github.com/nex3/haml/commit/2.2.5)

There were no changes made to Sass between versions 2.2.4 and 2.2.5.

## [2.2.4](http://github.com/nex3/haml/commit/2.2.4)

* Don't add `require 'rubygems'` to the top of init.rb when installed
  via `sass --rails`. This isn't necessary, and actually gets
  clobbered as soon as haml/template is loaded.

* Document the previously-undocumented {file:SASS_REFERENCE.md#line-option `:line` option},
  which allows the number of the first line of a Sass file to be set for error reporting.

## [2.2.3](http://github.com/nex3/haml/commit/2.2.3)

Sass 2.2.3 prints line numbers for warnings about selectors
with no properties.

## [2.2.2](http://github.com/nex3/haml/commit/2.2.2)

Sass 2.2.2 is a minor bug-fix release.
Notable changes include better parsing of mixin definitions and inclusions
and better support for Ruby 1.9.

## [2.2.1](http://github.com/nex3/haml/commit/2.2.1)

Sass 2.2.1 is a minor bug-fix release.

### Must Read!

* It used to be acceptable to use `-` immediately following variable names,
  without any whitespace in between (for example, `!foo-!bar`).
  This is now deprecated, so that in the future variables with hyphens
  can be supported. Surround `-` with spaces.

## [2.2.0](http://github.com/nex3/haml/commit/2.2.0)

The 2.2 release marks a significant step in the evolution of the Sass
language. The focus has been to increase the power of Sass to keep
your stylesheets maintainable by allowing new forms of abstraction to
be created within your stylesheets and the stylesheets provided by
others that you can download and import into your own. The fundamental
units of abstraction in Sass are variables and mixins. Please read
below for a list of changes:

### Must Read!

* Sass Comments (//) used to only comment out a single line. This was deprecated
  in 2.0.10 and starting in 2.2, Sass comments will comment out any lines indented
  under them. Upgrade to 2.0.10 in order to see deprecation warnings where this change
  affects you.

* Implicit Strings within SassScript are now deprecated and will be removed in 2.4.
  For example: `border= !width solid #00F` should now be written as `border: #{!width} solid #00F`
  or as `border= !width "solid" #00F`. After upgrading to 2.2, you will see deprecation warnings
  if you have sass files that use implicit strings.


### Sass Syntax Changes

#### Flexible Indentation

The indentation of Sass documents is now flexible. The first indent
that is detected will determine the indentation style for that
document. Tabs and spaces may never be mixed, but within a document,
you may choose to use tabs or a flexible number of spaces.

#### Multiline Sass Comments

Sass Comments (//) will now comment out whatever is indented beneath
them. Previously they were single line when used at the top level of a
document. Upgrading to the latest stable version will give you
deprecation warnings if you have silent comments with indentation
underneath them.

#### Mixin Arguments

Sass Mixins now accept any number of arguments. To define a mixin with
arguments, specify the arguments as a comma-delimited list of
variables like so:

    =my-mixin(!arg1, !arg2, !arg3)

As before, the definition of the mixin is indented below the mixin
declaration. The variables declared in the argument list may be used
and will be bound to the values passed to the mixin when it is
invoked.  Trailing arguments may have default values as part of the
declaration:

    =my-mixin(!arg1, !arg2 = 1px, !arg3 = blue)

In the example above, the mixin may be invoked by passing 1, 2 or 3
arguments to it. A similar syntax is used to invoke a mixin that
accepts arguments:

    div.foo
      +my-mixin(1em, 3px)

When a mixin has no required arguments, the parenthesis are optional.

The default values for mixin arguments are evaluated in the global
context at the time when the mixin is invoked, they may also reference
the previous arguments in the declaration. For example:

    !default_width = 30px
    =my-fancy-mixin(!width = !default_width, !height = !width)
      width= !width
      height= !height

    .default-box
      +my-fancy-mixin

    .square-box
      +my-fancy-mixin(50px)

    .rectangle-box
      +my-fancy-mixin(25px, 75px)

    !default_width = 10px
    .small-default-box
      +my-fancy-mixin
    

compiles to:

    .default-box {
      width: 30px;
      height: 30px; }

    .square-box {
      width: 50px;
      height: 50px; }

    .rectangle-box {
      width: 25px;
      height: 75px; }

    .small-default-box {
      width: 10px;
      height: 10px; }
    

### Sass, Interactive

The sass command line option -i now allows you to quickly and
interactively experiment with SassScript expressions. The value of the
expression you enter will be printed out after each line. Example:

    $ sass -i
    >> 5px
    5px
    >> 5px + 10px
    15px
    >> !five_pixels = 5px
    5px
    >> !five_pixels + 10px
    15px

### SassScript

The features of SassScript have been greatly enhanced with new control
directives, new fundamental data types, and variable scoping.

#### New Data Types

SassScript now has four fundamental data types:

1. Number
2. String
3. Boolean (New in 2.2)
4. Colors

#### More Flexible Numbers

Like JavaScript, SassScript numbers can now change between floating
point and integers. No explicit casting or decimal syntax is
required. When a number is emitted into a CSS file it will be rounded
to the nearest thousandth, however the internal representation
maintains much higher precision.

#### Improved Handling of Units

While Sass has long supported numbers with units, it now has a much
deeper understanding of them. The following are examples of legal
numbers in SassScript:

    0, 1000, 6%, -2px, 5pc, 20em, or 2foo.

Numbers of the same unit may always be added and subtracted. Numbers
that have units that Sass understands and finds comparable, can be
combined, taking the unit of the first number. Numbers that have
non-comparable units may not be added nor subtracted -- any attempt to
do so will cause an error. However, a unitless number takes on the
unit of the other number during a mathematical operation. For example:

    >> 3mm + 4cm
    43mm
    >> 4cm + 3mm
    4.3cm
    >> 3cm + 2in
    8.08cm
    >> 5foo + 6foo
    11foo
    >> 4% + 5px
    SyntaxError: Incompatible units: 'px' and '%'.
    >> 5 + 10px
    15px

Sass allows compound units to be stored in any intermediate form, but
will raise an error if you try to emit a compound unit into your css
file.

    >> !em_ratio = 1em / 16px
    0.063em/px
    >> !em_ratio * 32px
    2em
    >> !em_ratio * 40px
    2.5em

#### Colors

A color value can be declared using a color name, hexadecimal,
shorthand hexadecimal, the rgb function, or the hsl function. When
outputting a color into css, the color name is used, if any, otherwise
it is emitted as hexadecimal value. Examples:

    > #fff
    white
    >> white
    white
    >> #FFFFFF
    white
    >> hsl(180, 100, 100)
    white
    >> rgb(255, 255, 255)
    white
    >> #AAA
    #aaaaaa

Math on color objects is performed piecewise on the rgb
components. However, these operations rarely have meaning in the
design domain (mostly they make sense for gray-scale colors).

    >> #aaa + #123
    #bbccdd
    >> #333 * 2
    #666666

#### Booleans

Boolean objects can be created by comparison operators or via the
`true` and `false` keywords.  Booleans can be combined using the
`and`, `or`, and `not` keywords.

    >> true
    true
    >> true and false
    false
    >> 5 < 10
    true
    >> not (5 < 10)
    false
    >> not (5 < 10) or not (10 < 5)
    true
    >> 30mm == 3cm
    true
    >> 1px == 1em
    false

#### Strings

Unicode escapes are now allowed within SassScript strings.

### Control Directives

New directives provide branching and looping within a sass stylesheet
based on SassScript expressions. See the [Sass
Reference](SASS_REFERENCE.md.html#control_directives) for complete
details.

#### @for

The `@for` directive loops over a set of numbers in sequence, defining
the current number into the variable specified for each loop. The
`through` keyword means that the last iteration will include the
number, the `to` keyword means that it will stop just before that
number.

    @for !x from 1px through 5px
      .border-#{!x}
        border-width= !x

compiles to:

    .border-1px {
      border-width: 1px; }

    .border-2px {
      border-width: 2px; }

    .border-3px {
      border-width: 3px; }

    .border-4px {
      border-width: 4px; }

    .border-5px {
      border-width: 5px; }

#### @if / @else if / @else

The branching directives `@if`, `@else if`, and `@else` let you select
between several branches of sass to be emitted, based on the result of
a SassScript expression. Example:

    !type = "monster"
    p
      @if !type == "ocean"
        color: blue
      @else if !type == "matador"
        color: red
      @else if !type == "monster"
        color: green
      @else
        color: black

is compiled to:

    p {
      color: green; }

#### @while

The `@while` directive lets you iterate until a condition is
met. Example:

    !i = 6
    @while !i > 0
      .item-#{!i}
        width = 2em * !i
      !i = !i - 2

is compiled to:

    .item-6 {
      width: 12em; }

    .item-4 {
      width: 8em; }

    .item-2 {
      width: 4em; }

### Variable Scoping

The term "constant" has been renamed to "variable." Variables can be
declared at any scope (a.k.a. nesting level) and they will only be
visible to the code until the next outdent. However, if a variable is
already defined in a higher level scope, setting it will overwrite the
value stored previously.

In this code, the `!local_var` variable is scoped and hidden from
other higher level scopes or sibling scopes:

    .foo
      .bar
        !local_var = 1px
        width= !local_var
      .baz
        // this will raise an undefined variable error.
        width= !local_var
      // as will this
      width= !local_var

In this example, since the `!global_var` variable is first declared at
a higher scope, it is shared among all lower scopes:

    !global_var = 1px
    .foo
      .bar
        !global_var = 2px
        width= !global_var
      .baz
        width= !global_var
      width= !global_var

compiles to:

    .foo {
      width: 2px; }
      .foo .bar {
        width: 2px; }
      .foo .baz {
        width: 2px; }


### Interpolation

Interpolation has been added. This allows SassScript to be used to
create dynamic properties and selectors.  It also cleans up some uses
of dynamic values when dealing with compound properties. Using
interpolation, the result of a SassScript expression can be placed
anywhere:

    !x = 1
    !d = 3
    !property = "border"
    div.#{!property}
      #{!property}: #{!x + !d}px solid
      #{!property}-color: blue

is compiled to:

    div.border {
      border: 4px solid;
      border-color: blue; }

### Sass Functions

SassScript defines some useful functions that are called using the
normal CSS function syntax:

    p
      color = hsl(0, 100%, 50%)

is compiled to:

    #main {
      color: #ff0000; }

The following functions are provided: `hsl`, `percentage`, `round`,
`ceil`, `floor`, and `abs`.  You can define additional functions in
ruby.

See {Sass::Script::Functions} for more information.


### New Options

#### `:line_comments`

To aid in debugging, You may set the `:line_comments` option to
`true`. This will cause the sass engine to insert a comment before
each selector saying where that selector was defined in your sass
code.

#### `:template_location`

The {Sass::Plugin} `:template_location` option now accepts a hash of
sass paths to corresponding css paths. Please be aware that it is
possible to import sass files between these separate locations -- they
are not isolated from each other.

### Miscellaneous Features

#### `@debug` Directive

The `@debug` directive accepts a SassScript expression and emits the
value of that expression to the terminal (stderr).

Example:

    @debug 1px + 2px

During compilation the following will be printed:

    Line 1 DEBUG: 3px

#### Ruby 1.9 Support

Sass now fully supports Ruby 1.9.1. 

#### Sass Cache

By default, Sass caches compiled templates and
[partials](SASS_REFERENCE.md.html#partials).  This dramatically speeds
up re-compilation of large collections of Sass files, and works best
if the Sass templates are split up into separate files that are all
[`@import`](SASS_REFERENCE.md.html#import)ed into one large file.

Without a framework, Sass puts the cached templates in the
`.sass-cache` directory.  In Rails and Merb, they go in
`tmp/sass-cache`.  The directory can be customized with the
[`:cache_location`](#cache_location-option) option.  If you don't want
Sass to use caching at all, set the [`:cache`](#cache-option) option
to `false`.
