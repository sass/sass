# Haml Changelog

* Table of contents
{:toc}

## 2.4.0

### Object Reference Customization

It's now possible to customize the name used for {file:HAML_REFERENCE.md#object_reference_ object reference}
for a given object by implementing the `haml_object_ref` method on that object.
This method should return a string that will be used in place of the class name of the object
in the generated class and id.

### `--double-quote-attributes` Option

The Haml executable now has a `--double-quote-attributes` option (short form: `-q`)
that causes attributes to use a double-quote mark rather than single-quote.

## [2.2.4](http://github.com/nex3/haml/commit/2.2.4)

* Allow `end` to be used for silent script when it's followed by code.
  For example:

      - form_for do
        ...
      - end if @show_form

  This isn't very good style, but we're supporting it for consistency's sake.

* Don't add `require 'rubygems'` to the top of init.rb when installed
  via `haml --rails`. This isn't necessary, and actually gets
  clobbered as soon as haml/template is loaded.

## [2.2.3](http://github.com/nex3/haml/commit/2.2.3)

Haml 2.2.3 adds support for the JRuby bundling tools
for Google AppEngine, thanks to [Jan Ulbrich](http://github.com/ulbrich).

## [2.2.2](http://github.com/nex3/haml/commit/2.2.2)

Haml 2.2.2 is a minor bugfix release, with several notable changes.
First, {file:Haml/Helpers.html#haml_concat-instance_method `haml_concat`}
will now raise an error when used with `=`.
This has always been incorrect behavior,
and in fact has never actually worked.
The only difference is that now it will fail loudly.
Second, Ruby 1.9 is now more fully supported,
especially with the {file:HAML_REFERENCE.md#htmlstyle_attributes_ new attribute syntax}.
Third, filters are no longer escaped when the {file:HAML_REFERENCE.md#escape_html-option `:escape_html` option}
is enabled and `#{}` interpolation is used.

## [2.2.1](http://github.com/nex3/haml/commit/2.2.1)

Haml 2.2.1 is a minor bug-fix release.

## [2.2.0](http://github.com/nex3/haml/commit/2.2.0)

Haml 2.2 adds several new features to the language,
fixes several bugs, and dramatically improves performance
(particularly when running with {file:HAML_REFERENCE.md#ugly-option `:ugly`} enabled).

### Syntax Changes

#### HTML-Style Attribute Syntax

Haml 2.2 introduces a new syntax for attributes
based on the HTML syntax.
For example:

    %a(href="http://haml-lang.com" title="Haml's so cool!")
      %img(src="/images/haml.png" alt="Haml")

There are two main reasons for this.
First, the hash-style syntax is very Ruby-specific.
There are now [Haml implementations](http://en.wikipedia.org/wiki/Haml#Implementations)
in many languages, each of which has its own syntax for hashes
(or dicts or associative arrays or whatever they're called).
The HTML syntax will be adopted by all of them,
so you can feel comfortable using Haml in whichever language you need.

Second, the hash-style syntax is quite verbose.
`%img{:src => "/images/haml.png", :alt => "Haml"}`
is eight characters longer than `%img(src="/images/haml.png" alt="Haml")`.
Haml's supposed to be about writing templates quickly and easily;
HTML-style attributes should help out a lot with that.

Ruby variables can be used as attribute values by omitting quotes.
Local variables or instance variables can be used.
For example:

    %a(title=@title href=href) Stuff

This is the same as:

    %a{:title => @title, :href => href} Stuff

Because there are no commas separating attributes,
more complicated expressions aren't allowed.
You can use `#{}` interpolation to insert complicated expressions
in a HTML-style attribute, though:

    %span(class="widget_#{@widget.number}")

#### Multiline Attributes

In general, Haml tries to keep individual elements on a single line.
There is a [multiline syntax](#multiline) for overflowing onto further lines,
but it's intentionally awkward to use to encourage shorter lines.

However, there is one case where overflow is reasonable: attributes.
Often a tag will simply have a lot of attributes, and in this case
it makes sense to allow overflow.
You can now stretch an attribute hash across multiple lines:

    %script{:type => "text/javascript",
            :src  => "javascripts/script_#{2 + 7}"}

This also works for HTML-style attributes:

        %script(type="text/javascript"
            src="javascripts/script_#{2 + 7}")

Note that for hash-style attributes, the newlines must come after commas.

#### Universal interpolation

In Haml 2.0, you could use `==` to interpolate Ruby code
within a line of text using `#{}`.
In Haml 2.2, the `==` is unnecessary;
`#{}` can be used in any text.
For example:

    %p This is a really cool #{h what_is_this}!
    But is it a #{h what_isnt_this}?

In addition, to {file:HAML_REFERENCE.md#escaping_html escape} or {file:HAML_REFERENCE.md#unescaping_html unescape}
the interpolated code, you can just add `&` or `!`, respectively,
to the beginning of the line:

    %p& This is a really cool #{what_is_this}!
    & But is it a #{what_isnt_this}?

#### Flexible indentation

Haml has traditionally required its users to use two spaces of indentation.
This is the universal Ruby style, and still highly recommended.
However, Haml now allows any number of spaces or even tabs for indentation,
provided:

* Tabs and spaces are not mixed
* The indentation is consistent within a given document

### New Options

#### `:ugly`

The `:ugly` option is not technically new;
it was introduced in Haml 2.0 to make rendering deeply nested templates less painful.
However, it's been greatly empowered in Haml 2.2.
It now does all sorts of performance optimizations
that couldn't be done before,
and its use increases Haml's performance dramatically.
It's enabled by default in production in Rails,
and it's highly recommended for production environments
in other frameworks.

#### `:encoding` {#encoding-option}

This option specifies the encoding of the Haml template
when running under Ruby 1.9. It defaults to `Encoding.default_internal` or `"utf-8"`.
This is useful for making sure that you don't get weird
encoding errors when dealing with non-ASCII input data.

### Deprecations

#### `Haml::Helpers#puts`

This helper is being deprecated for the obvious reason
that it conflicts with the `Kernel#puts` method.
I'm ashamed I ever chose this name.
Use `haml_tag` instead and spare me the embarrassment.

#### `= haml_tag`

A lot of people accidentally use "`= haml_tag`".
This has always been wrong; `haml_tag` outputs directly to the template,
and so should be used as "`- haml_tag`".
Now it raises an error when you use `=`.

### Compatibility

#### Rails

Haml 2.2 is fully compatible with Rails,
from 2.0.6 to the latest revision of edge, 783db25.

#### Ruby 1.9

Haml 2.2 is also fully compatible with Ruby 1.9.
It supports Ruby 1.9-style attribute hashes,
and handles encoding-related issues
(see [the `:encoding` option](#encoding-option)).

### Filters

#### `:markdown`

There are numerous improvements to the Markdown filter.
No longer will Haml attempt to use RedCloth's inferior Markdown implementation.
Instead, it will look for all major Markdown implementations:
[RDiscount](http://github.com/rtomayko/rdiscount),
[RPeg-Markdown](http://github.com/rtomayko/rpeg-markdown),
[Maruku](http://maruku.rubyforge.org),
and [BlueCloth](www.deveiate.org/projects/BlueCloth).

#### `:cdata`

There is now a `:cdata` filter for wrapping text in CDATA tags.

#### `:sass`

The `:sass` filter now uses options set in {Sass::Plugin},
if they're available.

### Executables

#### `haml`

The `haml` executable now takes `-r` and `-I` flags
that act just like the same flags for the `ruby` executable.
This allows users to load helper files when using Haml
from the command line.

It also takes a `--debug` flag that causes it to spit out
the Ruby code that Haml generates from the template.
This is more for my benefit than anything,
but you may find it interesting.

#### `html2haml`

The `html2haml` executable has undergone significant improvements.
Many of these are bugfixes, but there are also a few features.
For one, it now understands CDATA tags and autodetects ERB files.
In addition, a line containing just "`- end`" is now a Haml error;
since it's not possible for `html2haml` to properly parse all Ruby blocks,
this acts as a signal for the author that there are blocks
to be dealt with.

### Miscellaneous

#### XHTML Mobile DTD

Haml 2.2 supports a DTD for XHTML Mobile: `!!! Mobile`.

#### YARD

All the documentation for Haml 2.2, including this changelog,
has been moved to [YARD](http://yard.soen.ca).
YARD is an excellent documentation system,
and allows us to write our documentation in [Maruku](http://maruku.rubyforge.org),
which is also excellent.
