# Intro to SCSS for Sass Users

Sass 3 introduces a new syntax known as SCSS
which is fully compatible with the syntax of CSS3,
while still supporting the full power of Sass.
This means that every valid CSS3 stylesheet
is a valid SCSS file with the same meaning.
In addition, SCSS understands most CSS hacks
and vendor-specific syntax, such as [IE's old `filter` syntax](http://msdn.microsoft.com/en-us/library/ms533754%28VS.85%29.aspx).

Since SCSS is a CSS extension,
everything that works in CSS works in SCSS.
This means that for a Sass user to understand it,
they need only understand how the Sass extensions work.
Most of these, such as variables, parent references, and directives work the same;
the only difference is that SCSS requires semicolons
and brackets instead of newlines and indentation.
For example, a simple rule in Sass:

    #sidebar
      width: 30%
      background-color: #faa

could be converted to SCSS just by adding brackets and semicolons:

    #sidebar {
      width: 30%;
      background-color: #faa;
    }

In addition, SCSS is completely whitespace-insensitive.
That means the above could also be written as:

    #sidebar {width: 30%; background-color: #faa}

There are some differences that are slightly more complicated.
These are detailed below.
Note, though, that SCSS uses all the
{file:SASS_CHANGELOG.md#3-0-0-syntax-changes syntax changes in Sass 3},
so make sure you understand those before going forward.

## Nested Selectors

To nest selectors, simply define a new ruleset
inside an existing ruleset:

    #sidebar {
      a { text-decoration: none; }
    }

Of course, white space is insignificant
and the last trailing semicolon is optional
so you can also do it like this:

    #sidebar { a { text-decoration: none } }

## Nested Properties

To nest properties,
simply create a new property set
after an existing property's colon:

    #footer {
      border: {
        width: 1px;
        color: #ccc;
        style: solid;
      }
    }

This compiles to:

    #footer {
      border-width: 1px;
      border-color: #cccccc;
      border-style: solid; }

## Mixins

A mixin is declared with the `@mixin` directive:

    @mixin rounded($amount) {
      -moz-border-radius: $amount;
      -webkit-border-radius: $amount;
      border-radius: $amount;
    }

A mixin is used with the `@include` directive:

    .box {
      border: 3px solid #777;
      @include rounded(0.5em);
    }

This syntax is also available in the indented syntax,
although the old `=` and `+` syntax still works.

This is rather verbose compared to the `=` and `+` characters used in Sass syntax.
This is because the SCSS format is designed for CSS compatibility rather than conciseness,
and creating new syntax when the CSS directive syntax already exists
adds new syntax needlessly and
could create incompatibilities with future versions of CSS.

## Comments

Like Sass, SCSS supports both comments that are preserved in the CSS output
and comments that aren't.
However, SCSS's comments are significantly more flexible.
It supports standard multiline CSS comments with `/* */`,
which are preserved where possible in the output.
These comments can have whatever formatting you like;
Sass will do its best to format them nicely.

SCSS also uses `//` for comments that are thrown away, like Sass.
Unlike Sass, though, `//` comments in SCSS may appear anywhere
and last only until the end of the line.

For example:

    /* This comment is
     * several lines long.
     * since it uses the CSS comment syntax,
     * it will appear in the CSS output. */
    body { color: black; }

    // These comments are only one line long each.
    // They won't appear in the CSS output,
    // since they use the single-line comment syntax.
    a { color: green; }

is compiled to:

    /* This comment is
     * several lines long.
     * since it uses the CSS comment syntax,
     * it will appear in the CSS output. */
    body {
      color: black; }

    a {
      color: green; }

## `@import`

The `@import` directive in SCSS functions just like that in Sass,
except that it takes a quoted string to import.
For example, this Sass:

    @import themes/dark
    @import font.sass

would be this SCSS:

    @import "themes/dark";
    @import "font.sass";
