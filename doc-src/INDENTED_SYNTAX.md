# Sass Indented Syntax

* Table of contents
{:toc}

Sass's indented syntax (also known simply as "Sass")
is designed to provide a more concise
and, for some, more aesthetically appealing alternative
to the CSS-based SCSS syntax.
It's not compatible with CSS;
instead of using `{` and `}` to delimit blocks of styles,
it uses indentation,
and instead of using semicolons to separate statements it uses newlines.
This usually leads to substantially less text
when saying the same thing.

Each statement in Sass, such as property declarations and selectors,
must be placed on its own line.
In addition, everything that would be within `{` and `}` after a statement
must be on a new line and indented one level deeper than that statement.
For example, this CSS:

    #main {
      color: blue;
      font-size: 0.3em;
    }

would be this Sass:

    #main
      color: blue
      font-size: 0.3em

Similarly, this SCSS:

    #main {
      color: blue;
      font-size: 0.3em;

      a {
        font: {
          weight: bold;
          family: serif;
        }
        &:hover {
          background-color: #eee;
        }
      }
    }

would be this Sass:

    #main
      color: blue
      font-size: 0.3em

      a
        font:
          weight: bold
          family: serif
        &:hover
          background-color: #eee

## Sass Syntax Differences

In general, most CSS and SCSS syntax
works straightforwardly in Sass
by using newlines instead of semicolons
and indentation instead of braces.
However, there are some cases where there are differences or subtleties,
which are detailed below.

## Property Synax

The indented syntax supports two ways of declaring CSS properties.
The first is just like CSS, except without the semicolon.
The second, however, places the colon *before* the property name.
For example:

    #main
      :color blue
      :font-size 0.3em

By default, both ways may be used.
However, the {file:SASS_REFERENCE.md#property_syntax-option `:property_syntax` option}
may be used to specify that only one property syntax is allowed.

### Multiline Selectors

Normally in the indented syntax, a single selector must take up a single line.
There is one exception, however:
selectors can contain newlines as long as they only appear after commas.
For example:

    .users #userTab,
    .posts #postTab
      width: 100px
      height: 30px

### Comments

Like everything else in the indented syntax,
comments are line-based.
This means that they don't work the same way as in SCSS.
They must take up an entire line,
and they also encompass all text nested beneath them.

Like SCSS, the indented syntax supports two kinds of comments.
Comments beginning with `/*` are preserved in the CSS output,
although unlike SCSS they don't require a closing `*/`.
Comments beginning with `//` are removed entirely.
For example:

    /* This comment will appear in the CSS output.
      This is nested beneath the comment,
      so it's part of it
    body
      color: black

    // This comment will not appear in the CSS output.
      This is nested beneath the comment as well,
      so it also won't appear
    a
      color: green

is compiled to:

    /* This comment will appear in the CSS output.
     * This is nested beneath the comment,
     * so it's part of it */
    body {
      color: black; }

    a {
      color: green; }

### `@import`

The `@import` directive in Sass does not require quotes, although they may be used.
For example, this SCSS:

    @import "themes/dark";
    @import "font.sass";

would be this Sass:

    @import themes/dark
    @import font.sass

### Mixin Directives

Sass supports shorthands for the `@mixin` and `@include` directives.
Instead of writing `@mixin`, you can use the character `=`;
instead of writing `@include`, you can use the character `+`.
For example:

    =large-text
      font:
        family: Arial
        size: 20px
        weight: bold
      color: #ff0000

    h1
      +large-text

is the same as:

    @mixin large-text
      font:
        family: Arial
        size: 20px
        weight: bold
      color: #ff0000

    h1
      @include large-text

## Deprecated Syntax

Since the indented syntax has been around for a while,
previous versions have made some syntactic decisions
that have since been changed.
Some of the old syntax still works, though,
so it's documented here.

**Note that this syntax is not recommended
for use in new Sass files**.
It will print a warning if it's used,
and it will be removed in a future version.

### `=` for Properties and Variables

`=` used to be used instead of `:` when setting variables
and when setting properties to SassScript values.
It has slightly different semantics than `:`;
see {file:SASS_CHANGELOG.md#3-0-0-sass-script-context this changelog entry} for details.

### `||=` for Default Variables

`||=` used to be used instead of `:` when setting the default value of a variable.
The `!default` flag was not used.
The variable value has the same semantics as `=`;
see {file:SASS_CHANGELOG.md#3-0-0-sass-script-context this changelog entry} for details.

### `!` Prefix for Variables

`!` used to be used as the variable prefix instead of `$`.
This had no difference in functionality;
it was a purely aesthetic change.
