# Intro to SCSS for Sass Users

In Sass 3, a new syntax is available.
This new syntax is called SCSS
and it is an extension of CSS
with the full power of Sass at your disposal.

## Nested Selectors

To nest selectors, simply define a new ruleset
inside a selector's ruleset.

    #sidebar {
      a { text-decoration: none; }
    }

Of course, white space is insignificant
and the last trailing semicolon is optional
so you can also do it like this:

    #sidebar { a { text-decoration: none } }

## Nested Properties

To nest properties,
simply create a new property set after the colon:

    #footer {
      border: {
        width: 1px;
        color: #ccc;
        style: solid;
      };
    }

Generates:

    #footer {
      border-width: 1px;
      border-color: #cccccc;
      border-style: solid; }


## Semi-colons

Just like in CSS, you can now separate properties with a semi-colon.

    a { text-decoration: none; font-weight: bold; }

## Mixins

A Mixin is declared with the @mixin directive:

    @mixin rounded($amount) {
      -moz-border-radius: $amount;
      -webkit-border-radius: $amount;
      border:radius: $amount;
    }

A Mixin is used with the @include directive:

    .box {
      border: 3px solid #777;
      @include rounded(0.5em);
    }

Existing sass users might complain that this is quite verbose
compared to the = and + special characters used in Sass syntax.
This is true, but CSS is a verbose format already,
and creating new syntax when the CSS directive syntax already exists
is unnecessary and might make supporting future versions of CSS more difficult or impossible.

## Variables

Note: This is true for the Sass syntax in Sass 3 as well.

Variables begin with a dollar sign ($) and can be assigned any legal css identifier.
Like properties in css, the value comes after a colon instead of an equals sign.

    $sidebar-bg : #f7f;
    $sidebar-border-style: solid;

## Defaulting Variables

Note: This is true for the Sass syntax in Sass 3 as well.

If you want to provide a default value for a variable
you can do so with the `!default` modifier.
For example:

    $sidebar-bg: #f7f;
    $previously-unset: blue !default; // is assigned the value of blue
    $sidebar-bg: red !default; // is still #f7f

In sass, this used to be done with the `||=` assignment operator.

# No More "Script Context"

Note: This is true for the Sass syntax in Sass 3 as well.

In Sass 2, you could only perform sass script operations in a script context.
That was after a `=` or within `#{}`.
In Sass 3, you can use SassScript in any value context.
And so use of the `=` operator has been deprecated.
However, you can still use `#{}` in all the places where you used to be able to.

## Identifiers & Strings

Note: This is true for the Sass syntax in Sass 3 as well.

In Sass 2, when the sass parser encountered `solid` in a script context
it would become a string just like if you had typed `"solid"`.
In Sass 2.2 this behavior was deprecated and strings required quotes in all cases --
but strings would drop their quotes when they arrived in the generated css file.

In Sass 3, all of this strangeness is gone.
If it's a string with quotes in Sass,
it'll be a string with quotes in the generated css,
and if it's an identifier without quotes in a sass file,
it'll be an identifier in the generated css file.
If you need to convert a string to an indentifier,
use the unquote() function or the quote() function to convert in the other direction.

One exception: When a string is returned from an interpolated escape,
it is automatically unquoted for you.
This is because interpolation is most commonly used with selectors
and should be largely unnecessary in values in sass 3.

    $a-string: "quoted";
    $an-ident: unquoted;
    my-#{$an-ident} { color: $a-string; } /* my-unquoted { color: "quoted"; } */
    my-#{$a-string} { color: $an-ident; } /* my-quoted { color: unquoted; } */
    my-#{$a-string} { color: #{$a-string}; } // my-quoted { color: quoted; }
