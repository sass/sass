# Sass (Syntactically Awesome StyleSheets)

* Table of contents
{:toc}

Sass is a meta-language on top of CSS
that's used to describe the style of a document
cleanly and structurally,
with more power than flat CSS allows.
Sass both provides a simpler, more elegant syntax for CSS
and implements various features that are useful
for creating manageable stylesheets.

## Features

* Whitespace active
* Well-formatted output
* Elegant input
* Feature-rich

## Using Sass

Sass can be used in three ways:
as a command-line tool,
as a standalone Ruby module,
and as a plugin for any Rack-enabled framework,
including Ruby on Rails and Merb.
Sass is bundled with Haml,
so if the Haml plugin or RubyGem is installed,
Sass will already be installed as a plugin or gem, respectively.
The first step for all of these is to install the Haml gem:

    gem install haml

To run Sass from the command line, just use

    sass input.sass output.css

Use `sass --help` for full documentation.
At the moment, the command-line tool doesn't support
updating everything in a directory
or automatically updating the CSS file when the Sass file changes.
To do that, check out the [Compass](http://compass-style.org/) Sass framework.

Using Sass in Ruby code is very simple.
After installing the Haml gem,
you can use it by running `require "sass"`
and using Sass::Engine like so:

    engine = Sass::Engine.new("#main\n  background-color: #0000ff")
    engine.render #=> "#main { background-color: #0000ff; }\n"

### Rack/Rails/Merb Plugin

To enable Sass as a Rails plugin, run

    haml --rails path/to/rails/app

To enable Sass in Merb,
add

    dependency "merb-haml"

to `config/dependencies.rb`.

To enable Sass in a Rack application,
add

    require 'sass/plugin/rack'
    use Sass::Plugin::Rack

to `config.ru`.

Sass stylesheets don't work the same as views.
They don't contain dynamic content,
so the CSS only needs to be generated when the Sass file has been updated.
By default, ".sass" files are placed in public/stylesheets/sass
(this can be customized with the [`:template_location`](#template_location-option) option).
Then, whenever necessary, they're compiled into corresponding CSS files in public/stylesheets.
For instance, public/stylesheets/sass/main.sass would be compiled to public/stylesheets/main.css.

### Caching

By default, Sass caches compiled templates and [partials](#partials).
This dramatically speeds up re-compilation of large collections of Sass files,
and works best if the Sass templates are split up into separate files
that are all [`@import`](#import)ed into one large file.

Without a framework, Sass puts the cached templates in the `.sass-cache` directory.
In Rails and Merb, they go in `tmp/sass-cache`.
The directory can be customized with the [`:cache_location`](#cache_location-option) option.
If you don't want Sass to use caching at all,
set the [`:cache`](#cache-option) option to `false`.

### Options

Options can be set by setting the {Sass::Plugin#options Sass::Plugin.options} hash
in `environment.rb` in Rails or `config.ru` in Rack...

    Sass::Plugin.options[:style] = :compact

...or by setting the `Merb::Plugin.config[:sass]` hash in `init.rb` in Merb...

    Merb::Plugin.config[:sass][:style] = :compact

...or by passing an options hash to {Sass::Engine#initialize}.
Available options are:

{#style-option} `:style`
: Sets the style of the CSS output.
  See [Output Style](#output_style).

{#property_syntax-option} `:property_syntax`
: Forces the document to use one syntax for properties.
  If the correct syntax isn't used, an error is thrown.
  `:new` forces the use of a colon or equals sign
  after the property name.
  For example: `color: #0f3`
  or `width = !main_width`.
  `:old` forces the use of a colon
  before the property name.
  For example: `:color #0f3`
  or `:width = !main_width`.
  By default, either syntax is valid.

{#cache-option} `:cache`
: Whether parsed Sass files should be cached,
  allowing greater speed. Defaults to true.

{#never_update-option} `:never_update`
: Whether the CSS files should never be updated,
  even if the template file changes.
  Setting this to true may give small performance gains.
  It always defaults to false.
  Only has meaning within Rack, Ruby on Rails, or Merb.

{#always_update-option} `:always_update`
: Whether the CSS files should be updated every
  time a controller is accessed,
  as opposed to only when the template has been modified.
  Defaults to false.
  Only has meaning within Rack, Ruby on Rails,x or Merb.

{#always_check-option} `:always_check`
: Whether a Sass template should be checked for updates every
  time a controller is accessed,
  as opposed to only when the server starts.
  If a Sass template has been updated,
  it will be recompiled and will overwrite the corresponding CSS file.
  Defaults to false in production mode, true otherwise.
  Only has meaning within Rack, Ruby on Rails, or Merb.

{#full_exception-option} `:full_exception`
: Whether an error in the Sass code
  should cause Sass to provide a detailed description
  within the generated CSS file.
  If set to true, the error will be displayed
  along with a line number and source snippet
  both as a comment in the CSS file
  and at the top of the page (in supported browsers).
  Otherwise, an exception will be raised in the Ruby code.
  Defaults to false in production mode, true otherwise.
  Only has meaning within Rack, Ruby on Rails, or Merb.

{#template_location-option} `:template_location`
: A path to the root sass template directory for your application.
  If a hash, `:css_location` is ignored and this option designates
  a mapping between input and output directories.
  May also be given a list of 2-element lists, instead of a hash.
  Defaults to `css_location + "/sass"`.
  Only has meaning within Rack, Ruby on Rails, or Merb.
  Note that if multiple template locations are specified, all
  of them are placed in the import path, allowing you to import
  between them.

{#css_location-option} `:css_location`
: The path where CSS output should be written to.
  This option is ignored when `:template_location` is a Hash.
  Defaults to `"./public/stylesheets"`.
  Only has meaning within Rack, Ruby on Rails, or Merb.

{#cache_location-option} `:cache_location`
: The path where the cached `sassc` files should be written to.
  Defaults to `"./tmp/sass-cache"` in Rails and Merb,
  or `"./.sass-cache"` otherwise.

{#unix_newlines-option} `:unix_newlines`
: If true, use Unix-style newlines when writing files.
  Only has meaning on Windows, and only when Sass is writing the files
  (in Rack, Rails, or Merb, or when using {Sass::Plugin} directly).

{#filename-option} `:filename`
: The filename of the file being rendered.
  This is used solely for reporting errors,
  and is automatically set when using Rack, Rails, or Merb.

{#line-option} `:line`
: The number of the first line of the Sass template.
  Used for reporting line numbers for errors.
  This is useful to set if the Sass template is embedded in a Ruby file.

{#load_paths-option} `:load_paths`
: An array of filesystem paths which should be searched
  for Sass templates imported with the [`@import`](#import) directive.
  This defaults to the working directory and, in Rack, Rails, or Merb,
  whatever `:template_location` is.

{#line_numbers-option} `:line_numbers`
: When set to true, causes the line number and file
  where a selector is defined to be emitted into the compiled CSS
  as a comment. Useful for debugging especially when using imports
  and mixins.
  This option may also be called `:line_comments`.

{#custom-option} `:custom`
: An option that's available for individual applications to set
  to make data available to {Sass::Script::Functions custom Sass functions}.

## CSS Rules

Rules in flat CSS have two elements:
the selector (e.g. `#main`, `div p`, `li a:hover`)
and the properties (e.g. `color: #00ff00;`, `width: 5em;`).
Sass has both of these,
as well as one additional element: nested rules.

### Rules and Selectors

However, some of the syntax is a little different.
The syntax for selectors is the same,
but instead of using brackets to delineate the properties that belong to a particular rule,
Sass uses indentation.
For example:

    #main p
      <property>
      <property>
      ...

Like CSS, you can stretch selectors over multiple lines.
However, unlike CSS, you can only do this if each line but the last
ends with a comma.
For example:

    .users #userTab,
    .posts #postsTab
      <property>

### Properties

There are two different ways to write CSS properties.
The first is very similar to the how you're used to writing them:
with a colon between the name and the value.
However, Sass properties don't have semicolons at the end;
each property is on its own line, so they aren't necessary.
For example:

    #main p
      color: #00ff00
      width: 97%

is compiled to:

    #main p {
      color: #00ff00;
      width: 97% }

The second syntax for properties is slightly different.
The colon is at the beginning of the property,
rather than between the name and the value,
so it's easier to tell what elements are properties just by glancing at them.
For example:

    #main p
      :color #00ff00
      :width 97%

is compiled to:

    #main p {
      color: #00ff00;
      width: 97% }

By default, either property syntax may be used.
If you want to force one or the other,
see the [`:property_syntax`](#property_syntax-option) option.

### Nested Rules

Rules can also be nested within each other.
This signifies that the inner rule's selector is a child of the outer selector.
For example:

    #main p
      color: #00ff00
      width: 97%

      .redbox
        background-color: #ff0000
        color: #000000

is compiled to:

    #main p {
      color: #00ff00;
      width: 97%; }
      #main p .redbox {
        background-color: #ff0000;
        color: #000000; }

This makes insanely complicated CSS layouts with lots of nested selectors very simple:

    #main
      width: 97%

      p, div
        font-size: 2em
        a
          font-weight: bold

      pre
        font-size: 3em

is compiled to:

    #main {
      width: 97%; }
      #main p, #main div {
        font-size: 2em; }
        #main p a, #main div a {
          font-weight: bold; }
      #main pre {
        font-size: 3em; }

### Referencing Parent Selectors: `&`

In addition to the default behavior of inserting the parent selector
as a CSS parent of the current selector
(e.g. above, `#main` is the parent of `p`),
you can have more fine-grained control over what's done with the parent selector
by using the ampersand character `&` in your selectors.

The ampersand is automatically replaced by the parent selector,
instead of having it prepended.
This allows you to cleanly create pseudo-classes:

    a
      font-weight: bold
      text-decoration: none
      &:hover
        text-decoration: underline
      &:visited
        font-weight: normal

Which would become:

    a {
      font-weight: bold;
      text-decoration: none; }
      a:hover {
        text-decoration: underline; }
      a:visited {
        font-weight: normal; }

It also allows you to add selectors at the base of the hierarchy,
which can be useuful for targeting certain styles to certain browsers:

    #main
      width: 90%
      #sidebar
        float: left
        margin-left: 20%
        .ie6 &
          margin-left: 40%

Which would become:

    #main {
      width: 90%; }
      #main #sidebar {
        float: left;
        margin-left: 20%; }
        .ie6 #main #sidebar {
          margin-left: 40%; }

### Property Namespaces

CSS has quite a few properties that are in "namespaces;"
for instance, `font-family`, `font-size`, and `font-weight`
are all in the `font` namespace.
In CSS, if you want to set a bunch of properties in the same namespace,
you have to type it out each time.
Sass offers a shortcut for this:
just write the namespace one,
then indent each of the sub-properties within it.
For example:

    .funky
      font:
        family: fantasy
        size: 30em
        weight: bold

is compiled to:

    .funky {
      font-family: fantasy;
      font-size: 30em;
      font-weight: bold; }

### Selector Escaping: `\`

In case, for whatever reason, you need to write a selector
that begins with a Sass-meaningful character,
you can escape it with a backslash (`\`).
For example:

    #main
      \+div
        clear: both

is compiled to:

    #main +div {
      clear: both; }

## Directives

Directives allow the author to directly issue instructions to the Sass compiler.
They're prefixed with an at sign, `@`,
followed by the name of the directive,
a space, and any arguments to it -
just like CSS directives.
For example:

    @import red.sass

Some directives can also control whether or how many times
a chunk of Sass is output.
Those are documented under Control Directives.

### `@import` {#import}

The `@import` directive works in a very similar way to the CSS import directive.
It can either compile to a literal CSS `@import` directive for a CSS file,
or it can import a Sass file.
If it imports a Sass file,
not only are the rules from that file included,
but all variables in that file are made available in the current file.

Sass looks for other Sass files in the working directory,
and the Sass file directory under Rack, Rails, or Merb.
Additional search directories may be specified
using the [`:load_paths`](#load_paths-option) option.

`@import` takes a filename with or without an extension.
If an extension isn't provided,
Sass will try to find a Sass file with the given basename in the load paths.

For example,

    @import foo.sass

or

    @import foo

would compile to

    .foo {
      color: #f00; }

whereas

    @import foo.css

would compile to

    @import "foo.css";

#### Partials {#partials}

If you have a Sass file that you want to import
but don't want to compile to a CSS file,
you can add an underscore to the beginning of the filename.
This will tell Sass not to compile it to a normal CSS file.
You can then refer to these files without using the underscore.

For example, you might have `_colors.sass`.
Then no `_colors.css` file would be created,
and you can do

    @import colors.sass

### `@debug`

The `@debug` directive prints the value of a SassScript expression
to standard error.
It's useful for debugging Sass files
that have complicated SassScript going on.
For example:

    @debug 10em + 12em

outputs:

    Line 1 DEBUG: 22em

### `@font-face`, `@media`, etc.

Sass behaves as you'd expect for normal CSS @-directives.
For example:

    @font-face
      font-family: "Bitstream Vera Sans"
      src: url(http://foo.bar/bvs)

compiles to:

    @font-face {
      font-family: "Bitstream Vera Sans";
      src: url(http://foo.bar/bvs); }

and

    @media print
      #sidebar
        display: none

      #main
        background-color: white

compiles to:

    @media print {
      #sidebar {
        display: none; }

      #main {
        background-color: white; } }

## Control Directives

SassScript supports basic control directives for looping and conditional evaluation.

### `@if`

The `@if` statement takes a SassScript expression
and prints the code nested beneath it if the expression returns
anything other than `false`:

    p
      @if 1 + 1 == 2
        border: 1px solid
      @if 5 < 3
        border: 2px dotted

is compiled to:

    p {
      border: 1px solid; }

The `@if` statement can be followed by several `@else if` statements
and one `@else` statement.
If the `@if` statement fails,
the `@else if` statements are tried in order
until one succeeds or the `@else` is reached.
For example:

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

### `@for`

The `@for` statement has two forms:
`@for <var> from <start> to <end>` or
`@for <var> from <start> through <end>`.
`<var>` is a variable name, like `!i`,
and `<start>` and `<end>` are SassScript expressions
that should return integers.

The `@for` statement sets `<var>` to each number
from `<start>` to `<end>`,
including `<end>` if `through` is used.
For example:

    @for !i from 1 through 3
      .item-#{!i}
        width = 2em * !i

is compiled to:

    .item-1 {
      width: 2em; }
    .item-2 {
      width: 4em; }
    .item-3 {
      width: 6em; }

### `@while`

The `@while` statement repeatedly loops over the nested
block until the statement evaluates to `false`. This can
be used to achieve more complex looping than the `@for`
statement is capable of.
For example:

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

## SassScript

In addition to the declarative templating system,
Sass supports a simple language known as SassScript
for dynamically computing CSS values and controlling
the styles and selectors that get emitted.

SassScript can be used as the value for a property
by using `=` instead of `:`.
For example:

    color = #123 + #234

is compiled to:

    color: #357;

For old-style properties, the `=` is added but the `:` is retained.
For example:

    :color = #123 + #234

is compiled to:

    color: #357;

### Interactive Shell

You can easily experiment with SassScript using the interactive shell.
To launch the shell run the sass command-line with the `-i` option. At the
prompt, enter any legal SassScript expression to have it evaluated
and the result printed out for you:

    $ sass -i
    >> "Hello, Sassy World!"
    "Hello, Sassy World!"
    >> 1px + 1px + 1px
    3px
    >> #777 + #777
    #eeeeee
    >> #777 + #888
    white

### Variables: `!`

The most straightforward way to use SassScript
is to set and reference variables.
Variables begin with exclamation marks,
and are set like so:

    !width = 5em

You can then refer to them by putting an equals sign
after your properties:

    #main
      width = !width

Variables that are first defined in a scoped context are only
available in that context.

### Data Types

SassScript supports four data types:
* numbers (e.g. `1.2`, `13`, `10px`)
* strings of text (e.g. `"foo"`, `"bar"`)
* colors (e.g. `blue`, `#04a3f9`)
* booleans (e.g. `true`, `false`)

Any text that doesn't fit into one of those types
in a SassScript context will cause an error:

    p
      !width = 5em
      // This will cause an error
        border = !width solid blue
      // Use one of the following forms instead:
      border = "#{!width} solid blue"
      border = !width "solid" "blue"

is compiled to:

    p {
      border: 5em solid blue;
      border: 5em solid blue; }


### Operations

SassScript supports the standard arithmetic operations on numbers
(`+`, `-`, `*`, `/`, `%`),
and will automatically convert between units if it can:

    p
      width = 1in + 8pt

is compiled to:

    p {
      width: 1.111in; }

Relational operators
(`<`, `>`, `<=`, `>=`)
are also supported for numbers,
and equality operators
(`==`, `!=`)
are supported for all types.

All arithmetic operations are supported for color values,
where they work piecewise.
This means that the operation is performed
on the red, green, and blue components in turn.
For example:

    p
      color = #010203 + #040506

computes `01 + 04 = 05`, `02 + 05 = 07`, and `03 + 06 = 09`,
and is compiled to:

    p {
      color: #050709; }

Arithmetic operations even work between numbers and colors,
also piecewise.
For example:

    p
      color = #010203 * 2

computes `01 * 2 = 02`, `02 * 2 = 04`, and `03 * 2 = 06`,
and is compiled to:

    p {
      color: #020406; }

The `+` operation can be used to concatenate strings:

    p
      cursor = "e" + "-resize"

is compiled to:

    p {
      cursor: e-resize; }

By default, if two values are placed next to one another,
they are concatenated with a space:

    p
      margin = 3px + 4px "auto"

is compiled to:

    p {
      margin: 7px auto; }

Within a string of text, #{} style interpolation can be used to
place dynamic values within the string:

    p
      border = "#{5px + 10px} solid #ccc"

Finally, SassScript supports `and`, `or`, and `not` operators
for boolean values.

### Parentheses

Parentheses can be used to affect the order of operations:

    p
      width = 1em + (2em * 3)

is compiled to:

    p {
      width: 7em; }

### Functions

SassScript defines some useful functions
that are called using the normal CSS function syntax:

    p
      color = hsl(0, 100%, 50%)

is compiled to:

    #main {
      color: #ff0000; }

The following functions are provided: `hsl`, `percentage`, `round`, `ceil`, `floor`, and `abs`.
You can define additional functions in ruby.

See {Sass::Script::Functions} for more information.

### Interpolation: `#{}`

You can also use SassScript variables in selectors
and property names using #{} interpolation syntax:

    !name = foo
    !attr = border
    p.#{!name}
      #{!attr}-color: blue

is compiled to:

    p.foo {
      border-color: blue; }

### Optional Assignment: `||=`

You can assign to variables if they aren't already assigned
using the `||=` assignment operator. This means that if the
variable has already been assigned to, it won't be re-assigned,
but if it doesn't have a value yet, it will be given one.

For example:

    !content = "First content"
    !content ||= "Second content?"
    !new_content ||= "First time reference"

    #main
      content = !content
      new-content = !new_content

is compiled to:

    #main {
      content: First content;
      new-content: First time reference; }

## Mixins

Mixins enable you to define groups of CSS properties and
then include them inline in any number of selectors
throughout the document. This allows you to keep your
stylesheets DRY and also avoid placing presentation
classes in your markup.

### Defining a Mixin: `=`

To define a mixin you use a slightly modified form of selector syntax.
For example the `large-text` mixin is defined as follows:

    =large-text
      font:
        family: Arial
        size: 20px
        weight: bold
      color: #ff0000

The initial `=` marks this as a mixin rather than a standard selector.
The CSS rules that follow won't be included until the mixin is referenced later on.
Anything you can put into a standard selector,
you can put into a mixin definition.
For example:

    =clearfix
      display: inline-block
      &:after
        content: "."
        display: block
        height: 0
        clear: both
        visibility: hidden
      * html &
        height: 1px

### Mixing It In: `+`

Inlining a defined mixin is simple,
just prepend a `+` symbol to the name of a mixin defined earlier in the document.
So to inline the `large-text` defined earlier,
we include the statment `+large-text` in our selector definition thus:

    .page-title
      +large-text
      padding: 4px
      margin:
        top: 10px

This will produce the following CSS output:

    .page-title {
      font-family: Arial;
      font-size: 20px;
      font-weight: bold;
      color: #ff0000;
      padding: 4px;
      margin-top: 10px; }

Any number of mixins may be defined and there is no limit on
the number that can be included in a particular selector.

Mixin definitions can also include references to other mixins.
For example:

    =compound
      +highlighted-background
      +header-text

    =highlighted-background
      background:
        color: #fc0
    =header-text
      font:
        size: 20px

Mixins that only define descendent selectors, can be safely mixed
into the top most level of a document.

### Arguments

Mixins can take arguments which can be used with SassScript:

    =sexy-border(!color)
      border:
        color = !color
        width: 1in
        style: dashed
    p
      +sexy-border("blue")

is compiled to:

    p {
      border-color: #0000ff;
      border-width: 1in;
      border-style: dashed; }

Mixins can also specify default values for their arguments:

    =sexy-border(!color, !width = 1in)
      border:
        color = !color
        width = !width
        style: dashed
    p
      +sexy-border("blue")

is compiled to:

    p {
      border-color: #0000ff;
      border-width: 1in;
      border-style: dashed; }

## Comments

Sass supports two sorts of comments:
those that show up in the CSS output
and those that don't.

### CSS Comments: `/*`

"Loud" comments are just as easy as silent ones.
These comments output to the document as CSS comments,
and thus use the same opening sequence: `/*`.
For example:

    /* A very awesome rule.
    #awesome.rule
      /* An equally awesome property.
      awesomeness: very

becomes

    /* A very awesome rule. */
    #awesome.rule {
      /* An equally awesome property. */
      awesomeness: very; }

You can also nest content beneath loud comments. For example:

    #pbj
      /* This rule describes
        the styling of the element
        that represents
        a peanut butter and jelly sandwich.
      background-image: url(/images/pbj.png)
      color: red

becomes

    #pbj {
      /* This rule describes
       * the styling of the element
       * that represents
       * a peanut butter and jelly sandwich. */
      background-image: url(/images/pbj.png);
      color: red; }

### Sass Comments: `//`

It's simple to add "silent" comments,
which don't output anything to the CSS document,
to a Sass document.
Simply use the familiar C-style notation for a one-line comment, `//`,
at the normal indentation level and all text following it won't be output.
For example:

    // A very awesome rule.
    #awesome.rule
      // An equally awesome property.
      awesomeness: very

becomes

    #awesome.rule {
      awesomeness: very; }

You can also nest text beneath a comment to comment out a whole block.
For example:

    // A very awesome rule
    #awesome.rule
      // Don't use these properties
        color: green
        font-size: 10em
      color: red

becomes

    #awesome.rule {
      color: red; }

## Output Style

Although the default CSS style that Sass outputs is very nice,
and reflects the structure of the document in a similar way that Sass does,
sometimes it's good to have other formats available.

Sass allows you to choose between four different output styles
by setting the `:style` option.
In Rack, Rails, and Merb, this is done by setting `Sass::Plugin.options[:style]`;
otherwise, it's done by passing an options hash with `:style` set.

### `:nested`

Nested style is the default Sass style,
because it reflects the structure of the document
in much the same way Sass does.
Each property has its own line,
but the indentation isn't constant.
Each rule is indented based on how deeply it's nested.
For example:

    #main {
      color: #fff;
      background-color: #000; }
      #main p {
        width: 10em; }

    .huge {
      font-size: 10em;
      font-weight: bold;
      text-decoration: underline; }

Nested style is very useful when looking at large CSS files
for the same reason Sass is useful for making them:
it allows you to very easily grasp the structure of the file
without actually reading anything.

### `:expanded`

Expanded is the typical human-made CSS style,
with each property and rule taking up one line.
Properties are indented within the rules,
but the rules aren't indented in any special way.
For example:

    #main {
      color: #fff;
      background-color: #000;
    }
    #main p {
      width: 10em;
    }

    .huge {
      font-size: 10em;
      font-weight: bold;
      text-decoration: underline;
    }

### `:compact`

Compact style, as the name would imply,
takes up less space than Nested or Expanded.
However, it's also harder to read.
Each CSS rule takes up only one line,
with every property defined on that line.
Nested rules are placed next to each other with no newline,
while groups of rules have newlines between them.
For example:

    #main { color: #fff; background-color: #000; }
    #main p { width: 10em; }

    .huge { font-size: 10em; font-weight: bold; text-decoration: underline; }

### `:compressed`

Compressed style takes up the minimum amount of space possible,
having no whitespace except that necessary to separate selectors
and a newline at the end of the file.
It's not meant to be human-readable.
For example:

    #main{color:#fff;background-color:#000}#main p{width:10em}.huge{font-size:10em;font-weight:bold;text-decoration:underline}
