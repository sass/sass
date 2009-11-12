# Haml Changelog

* Table of contents
{:toc}

## 2.4.0 (Unreleased)

### Object Reference Customization

It's now possible to customize the name used for {file:HAML_REFERENCE.md#object_reference_ object reference}
for a given object by implementing the `haml_object_ref` method on that object.
This method should return a string that will be used in place of the class name of the object
in the generated class and id.

### `--double-quote-attributes` Option

The Haml executable now has a `--double-quote-attributes` option (short form: `-q`)
that causes attributes to use a double-quote mark rather than single-quote.

### `haml-spec` Integration

We've added the cross-implementation tests from the [haml-spec](http://github.com/norman/haml-spec) project
to the standard Haml test suite, to be sure we remain compatible with the base functionality
of the many and varied [Haml implementations](http://en.wikipedia.org/wiki/Haml#Implementations).

### Ruby 1.9 Support

Haml and `html2haml` now produce more descriptive errors
when given a template with invalid byte sequences for that template's encoding,
including the line number and the offending character.

### `:css` Filter

Haml now supports a {file:HAML_REFERENCE.md#css-filter `:css` filter}
that surrounds the filtered text with `<style>` and CDATA tags.

### `html2haml` Improvements

* Ruby blocks within ERB are now supported.
  The Haml code is properly indented and the `end`s are removed.
  This includes methods with blocks and all language constructs
  such as `if`, `begin`, and `case`.
  For example:

      <% content_for :footer do %>
        <p>Hi there!</p>
      <% end %>

  is now transformed into:

      - content_for :footer do
        %p Hi there!

  Thanks to [Jack Chen](http://chendo.net) and [Dr. Nic Williams](http://drnicwilliams)
  for inspiring this and creating the first draft of the code.

* Inline HTML text nodes are now transformed into inline Haml text.
  For example, `<p>foo</p>` now becomes `%p foo`, whereas before it became:

      %p
        foo

  The same is true for inline comments,
  and inline ERB when running in ERB mode:
  `<p><%= foo %></p>` will now become `%p= foo`.

* ERB included within text is now transformed into Ruby interpolation.
  For example:

      <p>
        Foo <%= bar %> baz!
        Flip <%= bang %>.
      </p>

  is now transformed into:

      %p
        Foo #{bar} baz!
        Flip #{bang}.

* `<script>` tags are now transformed into `:javascript` filters,
  and `<style>` tags into `:css` filters.
  and indentation is preserved.
  For example:

      <script type="text/javascript">
        function foo() {
          return 12;
        }
      </script>

  is now transformed into:

      :javascript
        function foo() {
          return 12;
        }

* `<pre>` and `<textarea>` tags are now transformed into the `:preserve` filter.
  For example:

      <pre>Foo
        bar
          baz</pre>

  is now transformed into:

      %pre
        :preserve
          Foo
            bar
              baz

* Self-closing tags (such as `<br />`) are now transformed into
  self-closing Haml tags (like `%br/`).

* IE conditional comments are now properly parsed.

* Attributes are now output in a more-standard format,
  without spaces within the curly braces
  (e.g. `%p{:foo => "bar"}` as opposed to `%p{ :foo => "bar" }`).

* IDs and classes containing `#` and `.` are now output as string attributes
  (e.g. `%p{:class => "foo.bar"}`).

* Attributes are now sorted, to maintain a deterministic order.

* Multi-line ERB statements are now properly indented,
  and those without any content are removed.

## 2.2.14

* Don't print warnings when escaping attributes containing non-ASCII characters
  in Ruby 1.9.

* Don't crash when parsing an XHTML Strict doctype in `html2haml`.

## [2.2.13](http://github.com/nex3/haml/commit/2.2.13)

* Allow users to specify {file:HAML_REFERENCE.md#encoding_option `:encoding => "ascii-8bit"`}
  even for templates that include non-ASCII byte sequences.
  This makes Haml templates not crash when given non-ASCII input
  that's marked as having an ASCII encoding.

* Fixed an incompatibility with Hpricot 0.8.2, which is used for `html2haml`.

## [2.2.12](http://github.com/nex3/haml/commit/2.2.12)

There were no changes made to Sass between versions 2.2.11 and 2.2.12.

## [2.2.11](http://github.com/nex3/haml/commit/2.2.11)

* Fixed a bug with XSS protection where HTML escaping would raise an error
  if passed a non-string value.
  Note that this doesn't affect any HTML escaping when XSS protection is disabled.

* Fixed a bug in outer-whitespace nuking where whitespace-only Ruby strings
  blocked whitespace nuking beyond them.

* Use `ensure` to protect the resetting of the Haml output buffer
  against exceptions that are raised within the compiled Haml code.

* Fix an error line-numbering bug that appeared if an error was thrown
  within loud script (`=`).
  This is not the best solution, as it disables a few optimizations,
  but it shouldn't have too much effect and the optimizations
  will hopefully be re-enabled in version 2.4.

* Don't crash if the plugin skeleton is installed and `rake gems:install` is run.

* Don't use `RAILS_ROOT` directly.
  This no longer exists in Rails 3.0.
  Instead abstract this out as `Haml::Util.rails_root`.
  This changes makes Haml fully compatible with edge Rails as of this writing.

## [2.2.10](http://github.com/nex3/haml/commit/2.2.10)

* Fixed a bug where elements with dynamic attributes and no content
  would have too much whitespace between the opening and closing tag.

* Changed `rails/init.rb` away from loading `init.rb` and instead
  have it basically copy the content.
  This allows us to transfer the proper binding to `Haml.init_rails`.

* Make sure Haml only tries to enable XSS protection integration
  once all other plugins are loaded.
  This allows it to work properly when Haml is a gem
  and the `rails_xss` plugin is being used.

* Mark the return value of Haml templates as HTML safe.
  This makes Haml partials work with Rails' XSS protection.

## [2.2.9](http://github.com/nex3/haml/commit/2.2.9)

* Fixed a bug where Haml's text was concatenated to the wrong buffer
  under certain circumstances.
  This was mostly an issue under Rails when using methods like `capture`.

* Fixed a bug where template text was escaped when there was interpolation in a line
  and the `:escape_html` option was enabled. For example:

      Foo &lt; Bar #{"<"} Baz

  with `:escape_html` used to render as

      Foo &amp;lt; Bar &lt; Baz

  but now renders as

      Foo &lt; Bar &lt; Baz

### Rails XSS Protection

Haml 2.2.9 supports the XSS protection in Rails versions 2.3.5+.
There are several components to this:

* If XSS protection is enabled, Haml's {file:HAML_REFERENCE.md#escape_html-option `:escape_html`}
  option is set to `true` by default.

* Strings declared as HTML safe won't be escaped by Haml,
  including the {file:Haml/Helpers.html#html_escape-instance_method `#html_escape`} helper
  and `&=` if `:escape_html` has been disabled.

* Haml helpers that generate HTML are marked as HTML safe,
  and will escape their input if it's not HTML safe.

## [2.2.8](http://github.com/nex3/haml/commit/2.2.8)

* Fixed a potential XSS issue with HTML escaping and wacky Unicode nonsense.
  This is the same as [the issue fixed in Rails](http://groups.google.com/group/rubyonrails-security/browse_thread/thread/48ab3f4a2c16190f) a bit ago.

## [2.2.7](http://github.com/nex3/haml/commit/2.2.7)

* Fixed an `html2haml` issue where ERB attribute values
  weren't HTML-unescaped before being transformed into Haml.

* Fixed an `html2haml` issue where `#{}` wasn't escaped
  before being transformed into Haml.

* Add `<code>` to the list of tags that's
  {file:HAML_REFERENCE.md#preserve-option automatically whitespace-preserved}.

* Fixed a bug with `end` being followed by code in silent scripts -
  it no longer throws an error when it's nested beneath tags.

* Fixed a bug with inner whitespace-nuking and conditionals.
  The `else` et al. clauses of conditionals are now properly
  whitespace-nuked.

## [2.2.6](http://github.com/nex3/haml/commit/2.2.6)

* Made the error message when unable to load a dependency for html2haml
  respect the `--trace` option.

* Don't crash when the `__FILE__` constant of a Ruby file is a relative path,
  as apparently happens sometimes in TextMate
  (thanks to [Karl Varga](http://github.com/kjvarga)).

* Add "Sass" to the `--version` string for the executables.

* Raise an exception when commas are omitted in static attributes
  (e.g. `%p{:foo => "bar" :baz => "bang"}`).

## [2.2.5](http://github.com/nex3/haml/commit/2.2.5)

* Got rid of trailing whitespace produced when opening a conditional comment
  (thanks to [Norman Clarke](http://blog.njclarke.com/)).

* Fixed CSS id concatenation when a numeric id is given as an attribute.
  (thanks to [Norman Clarke](http://blog.njclarke.com/)).
  
* Fixed a couple bugs with using "-end" in strings.

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
