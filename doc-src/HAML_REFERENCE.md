# Haml (XHTML Abstraction Markup Language)

* Table of contents
{:toc}

Haml is a markup language
that's used to cleanly and simply describe the XHTML of any web document,
without the use of inline code.
Haml functions as a replacement
for inline page templating systems such as PHP, ERB, and ASP.
However, Haml avoids the need for explicitly coding XHTML into the template,
because it is actually an abstract description of the XHTML,
with some code to generate dynamic content.

## Features

* Whitespace active
* Well-formatted markup
* DRY
* Follows CSS conventions
* Integrates Ruby code
* Implements Rails templates with the .haml extension

## Using Haml

Haml can be used in three ways:
as a command-line tool,
as a plugin for Ruby on Rails,
and as a standalone Ruby module.
The first step for all of these is to install the Haml gem:

    gem install haml

To run Haml from the command line, just use

    haml input.haml output.html

Use `haml --help` for full documentation.

### Rails/Merb Plugin {#plugin}

To enable Haml as a Rails plugin, run

    haml --rails path/to/rails/app

Once it's installed, all view files with the `".html.haml"` extension
will be compiled using Haml.
Haml is enabled by default in Merb.

You can access instance variables in Haml templates
the same way you do in ERB templates.
Helper methods are also available in Haml templates.
For example (this example uses Rails, but the principle for Merb is the same):

    # file: app/controllers/movies_controller.rb

    class MoviesController < ApplicationController
      def index
        @title = "Teen Wolf"
      end
    end

    -# file: app/views/movies/index.haml

    #content
     .title
       %h1= @title
       = link_to 'Home', home_url

may be compiled to:

    <div id='content'>
      <div class='title'>
        <h1>Teen Wolf</h1>
        <a href='/'>Home</a>
      </div>
    </div>

#### Rails XSS Protection

Haml supports Rails' XSS protection scheme,
which was introduced in Rails 2.3.5+ and is enabled by default in 3.0.0+.
If it's enabled, Haml's [`:escape_html`](#escape_html-option)
option is set to `true` by default -
like in ERB, all strings printed to a Haml template are escaped by default.
Also like ERB, strings marked as HTML safe are not escaped.
Haml also has [its own syntax for printing a raw string to the template](#unescaping_html).

If the `:escape_html` option is set to false when XSS protection is enabled,
Haml doesn't escape Ruby strings by default.
However, if a string marked HTML-safe is passed to [Haml's escaping syntax](#escaping_html),
it won't be escaped.

Finally, all the {file:Haml/Helpers.html Haml helpers} that return strings
that are known to be HTML safe are marked as such.
In addition, string input is escaped unless it's HTML safe.

### Ruby Module

Haml can also be used completely separately from Rails and ActionView.
To do this, install the gem with RubyGems:

    gem install haml

You can then use it by including the "haml" gem in Ruby code,
and using {Haml::Engine} like so:

    engine = Haml::Engine.new("%p Haml code!")
    engine.render #=> "<p>Haml code!</p>\n"

### Options

Options can be set by setting the {Haml::Template#options Haml::Template.options} hash
in `environment.rb` in Rails...

    Haml::Template.options[:format] = :html5

...or by setting the `Merb::Plugin.config[:haml]` hash in `init.rb` in Merb...

    Merb::Plugin.config[:haml][:format] = :html5

...or by passing an options hash to {Haml::Engine#initialize}.
Available options are:

{#format-option} `:format`
: Determines the output format. The default is `:xhtml`.
  Other options are `:html4` and `:html5`, which are
  identical to `:xhtml` except there are no self-closing tags,
  the XML prolog is ignored and correct DOCTYPEs are generated.

{#escape_html-option} `:escape_html`
: Sets whether or not to escape HTML-sensitive characters in script.
  If this is true, `=` behaves like [`&=`](#escaping_html);
  otherwise, it behaves like [`!=`](#unescaping_html).
  Note that if this is set, `!=` should be used for yielding to subtemplates
  and rendering partials.
  See also [Escaping HTML](#escaping_html) and [Unescaping HTML](#unescaping_html)
  Defaults to false.

{#ugly-option} `:ugly`
: If set to `true`, Haml makes no attempt to properly
  indent or format the HTML output.
  This causes the rendering to be done much quicker than it would otherwise,
  but makes viewing the source unpleasant.
  Defaults to `false`, but is set to `true`
  in production for Rails.

{#suppress_eval-option} `:suppress_eval`
: Whether or not attribute hashes and Ruby scripts
  designated by `=` or `~` should be
  evaluated. If this is `true`, said scripts are
  rendered as empty strings. Defaults to `false`.

{#attr_wrapper-option} `:attr_wrapper`
: The character that should wrap element attributes.
  This defaults to `'` (an apostrophe). Characters
  of this type within the attributes will be escaped
  (e.g. by replacing them with `&apos;`) if
  the character is an apostrophe or a quotation mark.

{#filename-option} `:filename`
: The name of the Haml file being parsed.
  This is only used as information when exceptions are raised.
  This is automatically assigned when working through ActionView,
  so it's really only useful for the user to assign
  when dealing with Haml programatically.

{#line-option} `:line`
: The line offset of the Haml template being parsed.
  This is useful for inline templates,
  similar to the last argument to `Kernel#eval`.

{#autoclose-option} `:autoclose`
: A list of tag names that should be automatically self-closed
  if they have no content.
  Defaults to `['meta', 'img', 'link', 'br', 'hr', 'input', 'area', 'param', 'col', 'base']`.

{#preserve-option} `:preserve`
: A list of tag names that should automatically have their newlines preserved
  using the {Haml::Helpers#preserve} helper.
  This means that any content given on the same line as the tag will be preserved.
  For example, `%textarea= "Foo\nBar"` compiles to `<textarea>Foo&#x000A;Bar</textarea>`.
  Defaults to `['textarea', 'pre']`.
  See also [Whitespace Preservation](#whitespace_preservation).

{#encoding-option} `:encoding`
: The encoding to use for the HTML output.
  Only available in Ruby 1.9 or higher.
  This can be a string or an `Encoding` Object.
  Note that Haml **does not** automatically re-encode Ruby values;
  any strings coming from outside the application should be converted
  before being passed into the Haml template.
  Defaults to `Encoding.default_internal` or, if that's not set, `"utf-8"`.
  <br/><br/> <!-- There's no better way to do a paragraph break in a dl in Maruku -->
  Many Ruby database drivers are not yet Ruby 1.9 compatible;
  in particular, they return strings marked as ASCII-encoded
  even when those strings contain non-ASCII characters (such as UTF-8).
  **This will cause encoding errors** if the Haml encoding isn't set to `"ascii-8bit"`.
  To solve this, either call `#force_encoding` on all the strings returned from the database,
  set `:encoding` to `"ascii-8bit"`, or try to get the authors of the database drivers
  to make them Ruby 1.9 compatible.

## Plain Text

A substantial portion of any HTML document is its content,
which is plain old text.
Any Haml line that's not interpreted as something else
is taken to be plain text, and passed through unmodified.
For example:

    %gee
      %whiz
        Wow this is cool!

is compiled to:

    <gee>
      <whiz>
        Wow this is cool!
      </whiz>
    </gee>

Note that HTML tags are passed through unmodified as well.
If you have some HTML you don't want to convert to Haml,
or you're converting a file line-by-line,
you can just include it as-is.
For example:

    %p
      <div id="blah">Blah!</div>

is compiled to:

    <p>
      <div id="blah">Blah!</div>
    </p>

### Escaping: `\`

The backslash character escapes the first character of a line,
allowing use of otherwise interpreted characters as plain text.
For example:

    %title
      = @title
      \= @title

is compiled to:

    <title>
      MyPage
      = @title
    </title>

## HTML Elements


### Element Name: `%`

The percent character is placed at the beginning of a line.
It's followed immediately by the name of an element,
then optionally by modifiers (see below), a space,
and text to be rendered inside the element.
It creates an element in the form of `<element></element>`.
For example:

    %one
      %two
        %three Hey there

is compiled to:

    <one>
      <two>
        <three>Hey there</three>
      </two>
    </one>

Any string is a valid element name;
Haml will automatically generate opening and closing tags for any element.

### Attributes: `{}` or `()` {#attributes}

Brackets represent a Ruby hash
that is used for specifying the attributes of an element.
It is literally evaluated as a Ruby hash,
so logic will work in it and local variables may be used.
Quote characters within the attribute
will be replaced by appropriate escape sequences.
The hash is placed after the tag is defined.
For example:

    %html{:xmlns => "http://www.w3.org/1999/xhtml", "xml:lang" => "en", :lang => "en"}

is compiled to:

    <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'></html>

Attribute hashes can also be stretched out over multiple lines
to accomidate many attributes.
However, newlines may only be placed immediately after commas.
For example:

    %script{:type => "text/javascript",
            :src  => "javascripts/script_#{2 + 7}"}

is compiled to:

    <script src='javascripts/script_9' type='text/javascript'></script>

#### HTML-style Attributes: `()`

Haml also supports a terser, less Ruby-specific attribute syntax
based on HTML's attributes.
These are used with parentheses instead of brackets, like so:

    %html(xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en")

Ruby variables can be used by omitting the quotes.
Local variables or instance variables can be used.
For example:

    %a(title=@title href=href) Stuff

This is the same as:

    %a{:title => @title, :href => href} Stuff

Because there are no commas separating attributes, though,
more complicated expressions aren't allowed.
For those you'll have to use the `{}` syntax.
You can, however, use both syntaxes together:

    %a(title=@title){:href => @link.href} Stuff

You can also use `#{}` interpolation to insert complicated expressions
in a HTML-style attribute:

    %span(class="widget_#{@widget.number}")

HTML-style attributes can be stretched across multiple lines
just like hash-style attributes:

    %script(type="text/javascript"
            src="javascripts/script_#{2 + 7}")

#### Attribute Methods

A Ruby method call that returns a hash
can be substituted for the hash contents.
For example, {Haml::Helpers} defines the following method:

    def html_attrs(lang = 'en-US')
      {:xmlns => "http://www.w3.org/1999/xhtml", 'xml:lang' => lang, :lang => lang}
    end

This can then be used in Haml, like so:

    %html{html_attrs('fr-fr')}

This is compiled to:

    <html lang='fr-fr' xml:lang='fr-fr' xmlns='http://www.w3.org/1999/xhtml'>
    </html>

You can use as many such attribute methods as you want
by separating them with commas,
like a Ruby argument list.
All the hashes will me merged together, from left to right.
For example, if you defined

    def hash1
      {:bread => 'white', :filling => 'peanut butter and jelly'}
    end

    def hash2
      {:bread => 'whole wheat'}
    end

then

    %sandwich{hash1, hash2, :delicious => true}/

would compile to:

    <sandwich bread='whole wheat' delicious='true' filling='peanut butter and jelly' />

Note that the Haml attributes list has the same syntax as a Ruby method call.
This means that any attribute methods must come before the hash literal.

Attribute methods aren't supported for HTML-style attributes.

#### Boolean Attributes

Some attributes, such as "checked" for `input` tags or "selected" for `option` tags,
are "boolean" in the sense that their values don't matter -
it only matters whether or not they're present.
In HTML (but not XHTML), these attributes can be written as

    <input selected>

To do this in Haml using hash-style attributes, just assign a Ruby
`true` value to the attribute:

    %input{:selected => true}

In XHTML, the only valid value for these attributes is the name of the
attribute.  Thus this will render in XHTML as

    <input selected='selected'>

To set these attributes to false, simply assign them to a Ruby false value.
In both XHTML and HTML

    %input{:selected => false}

will just render as

    <input>

HTML-style boolean attributes can be written just like HTML:

    %input(selected)

or using `true` and `false`:

    %input(selected=true)

### Class and ID: `.` and `#`

The period and pound sign are borrowed from CSS.
They are used as shortcuts to specify the `class`
and `id` attributes of an element, respectively.
Multiple class names can be specified in a similar way to CSS,
by chaining the class names together with periods.
They are placed immediately after the tag and before an attributes hash.
For example:

    %div#things
      %span#rice Chicken Fried
      %p.beans{ :food => 'true' } The magical fruit
      %h1.class.otherclass#id La La La

is compiled to:

    <div id='things'>
      <span id='rice'>Chicken Fried</span>
      <p class='beans' food='true'>The magical fruit</p>
      <h1 class='class otherclass' id='id'>La La La</h1>
    </div>

And,

    #content
      .articles
        .article.title Doogie Howser Comes Out
        .article.date 2006-11-05
        .article.entry
          Neil Patrick Harris would like to dispel any rumors that he is straight

is compiled to:

    <div id='content'>
      <div class='articles'>
        <div class='article title'>Doogie Howser Comes Out</div>
        <div class='article date'>2006-11-05</div>
        <div class='article entry'>
          Neil Patrick Harris would like to dispel any rumors that he is straight
        </div>
      </div>
    </div>

#### Implicit Div Elements

Because divs are used so often, they're the default elements.
If you only define a class and/or id using `.` or `#`,
a div is automatically used.
For example:

    #collection
      .item
        .description What a cool item!

is the same as:

    %div#collection
      %div.item
        %div.description What a cool item!

and is compiled to:

    <div id='collection'>
      <div class='item'>
        <div class='description'>What a cool item!</div>
      </div>
    </div>

### Self-Closing Tags: `/`

The forward slash character, when placed at the end of a tag definition,
causes the tag to be self-closed.
For example:

    %br/
    %meta{'http-equiv' => 'Content-Type', :content => 'text/html'}/

is compiled to:

    <br />
    <meta http-equiv='Content-Type' content='text/html' />

Some tags are automatically closed, as long as they have no content.
`meta`, `img`, `link`, `script`, `br`, and `hr` tags are closed by default.
This list can be customized by setting the [`:autoclose`](#autoclose-option) option.
For example:

    %br
    %meta{'http-equiv' => 'Content-Type', :content => 'text/html'}

is also compiled to:

    <br />
    <meta http-equiv='Content-Type' content='text/html' />

### Whitespace Removal: `>` and `<`

`>` and `<` give you more control over the whitespace near a tag.
`>` will remove all whitespace surrounding a tag,
while `<` will remove all whitespace immediately within a tag.
You can think of them as alligators eating the whitespace:
`>` faces out of the tag and eats the whitespace on the outside,
and `<` faces into the tag and eats the whitespace on the inside.
They're placed at the end of a tag definition,
after class, id, and attribute declarations
but before `/` or `=`.
For example:

    %blockquote<
      %div
        Foo!

is compiled to:

    <blockquote><div>
      Foo!
    </div></blockquote>

And:

    %img
    %img>
    %img

is compiled to:

    <img /><img /><img />

And:

    %p<= "Foo\nBar"

is compiled to:

    <p>Foo
    Bar</p>

And finally:

    %img
    %pre><
      foo
      bar
    %img

is compiled to:

    <img /><pre>foo
    bar</pre><img />

### Object Reference: `[]`

Square brackets follow a tag definition and contain a Ruby object
that is used to set the class and id of that tag.
The class is set to the object's class
(transformed to use underlines rather than camel case)
and the id is set to the object's class, followed by its id.
Because the id of an object is normally an obscure implementation detail,
this is most useful for elements that represent instances of Models.
Additionally, the second argument (if present) will be used as a prefix for
both the id and class attributes.
For example:

    # file: app/controllers/users_controller.rb

    def show
      @user = CrazyUser.find(15)
    end

    -# file: app/views/users/show.haml

    %div[@user, :greeting]
      %bar[290]/
      Hello!

is compiled to:

    <div class='greeting_crazy_user' id='greeting_crazy_user_15'>
      <bar class='fixnum' id='fixnum_581' />
      Hello!
    </div>

If you require that the class be something other than the underscored
object's class, you can implement the `haml_object_ref` method on the object.

    # file: app/models/crazy_user.rb

    class CrazyUser < ActiveRecord::Base
      def haml_object_ref
        "a_crazy_user"
      end
    end

    -# file: app/views/users/show.haml

    %div[@user]
      Hello!

is compiled to:

    <div class='a_crazy_user' id='a_crazy_user_15'>
      Hello!
    </div>


## Doctype: `!!!`

When describing HTML documents with Haml,
you can have a document type or XML prolog generated automatically
by including the characters `!!!`.
For example:

    !!! XML
    !!!
    %html
      %head
        %title Myspace
      %body
        %h1 I am the international space station
        %p Sign my guestbook

is compiled to:

    <?xml version='1.0' encoding='utf-8' ?>
    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
    <html>
      <head>
        <title>Myspace</title>
      </head>
      <body>
        <h1>I am the international space station</h1>
        <p>Sign my guestbook</p>
      </body>
    </html>

You can also specify the version and type of XHTML after the `!!!`.
XHTML 1.0 Strict, Transitional, and Frameset and XHTML 1.1 are supported.
The default version is 1.0 and the default type is Transitional.
For example:

    !!! 1.1

is compiled to:

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">

and

    !!! Strict

is compiled to:

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

while 

    !!! Basic

is compiled to:

    <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd"> 

and
  
    !!! Mobile

is compiled to: 

    <!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">

If you're not using the UTF-8 character set for your document,
you can specify which encoding should appear
in the XML prolog in a similar way.
For example:

    !!! XML iso-8859-1

is compiled to:

    <?xml version='1.0' encoding='iso-8859-1' ?>

## Comments

Haml supports two sorts of comments:
those that show up in the HTML output
and those that don't.

### HTML Comments: `/`

The forward slash character, when placed at the beginning of a line,
wraps all text after it in an HTML comment.
For example:

    %peanutbutterjelly
      / This is the peanutbutterjelly element
      I like sandwiches!

is compiled to:

    <peanutbutterjelly>
      <!-- This is the peanutbutterjelly element -->
      I like sandwiches!
    </peanutbutterjelly>

The forward slash can also wrap indented sections of code. For example:

    /
      %p This doesn't render...
      %div
        %h1 Because it's commented out!

is compiled to:

    <!--
      <p>This doesn't render...</p>
      <div>
        <h1>Because it's commented out!</h1>
      </div>
    -->

#### Conditional Comments: `/[]`

You can also use [Internet Explorer conditional comments](http://www.quirksmode.org/css/condcom.html)
by enclosing the condition in square brackets after the `/`.
For example:

    /[if IE]
      %a{ :href => 'http://www.mozilla.com/en-US/firefox/' }
        %h1 Get Firefox

is compiled to:

    <!--[if IE]>
      <a href='http://www.mozilla.com/en-US/firefox/'>
        <h1>Get Firefox</h1>
      </a>
    <![endif]-->

### Haml Comments: `-#`

The hyphen followed immediately by the pound sign
signifies a silent comment.
Any text following this isn't rendered in the resulting document
at all.

For example:

    %p foo
    -# This is a comment
    %p bar

is compiled to:

    <p>foo</p>
    <p>bar</p>

You can also nest text beneath a silent comment.
None of this text will be rendered.
For example:

    %p foo
    -#
      This won't be displayed
        Nor will this
    %p bar

is compiled to:

    <p>foo</p>
    <p>bar</p>

## Ruby Evaluation

### Inserting Ruby: `=`

The equals character is followed by Ruby code.
This code is evaluated and the output is inserted into the document.
For example:

    %p
      = ['hi', 'there', 'reader!'].join " "
      = "yo"

is compiled to:

    <p>
      hi there reader!
      yo
    </p>

If the [`:escape_html`](#escape_html-option) option is set, `=` will sanitize any
HTML-sensitive characters generated by the script. For example:

    = '<script>alert("I\'m evil!");</script>'

would be compiled to

    &lt;script&gt;alert(&quot;I'm evil!&quot;);&lt;/script&gt;

`=` can also be used at the end of a tag to insert Ruby code within that tag.
For example:

    %p= "hello"

would be compiled to

    <p>hello</p>

Note that it's illegal to nest code within a tag that ends with `=`.

### Running Ruby: `-`

The hyphen character is also followed by Ruby code.
This code is evaluated but *not* inserted into the document.

**It is not recommended that you use this widely;
almost all processing code and logic should be restricted
to the Controller, the Helper, or partials.**

For example:

    - foo = "hello"
    - foo << " there"
    - foo << " you!"
    %p= foo

is compiled to:

    <p>
      hello there you!
    </p>

#### Ruby Blocks

Ruby blocks, like XHTML tags, don't need to be explicitly closed in Haml.
Rather, they're automatically closed, based on indentation.
A block begins whenever the indentation is increased
after a Ruby evaluation command.
It ends when the indentation decreases
(as long as it's not an `else` clause or something similar).
For example:

    - (42...47).each do |i|
      %p= i
    %p See, I can count!

is compiled to:

    <p>
      42
    </p>
    <p>
      43
    </p>
    <p>
      44
    </p>
    <p>
      45
    </p>
    <p>
      46
    </p>

Another example:

    %p
      - case 2
      - when 1
        = "1!"
      - when 2
        = "2?"
      - when 3
        = "3."

is compiled to:

    <p>
      2?
    </p>

### Whitespace Preservation: `~` {#tilde}

`~` works just like `=`, except that it runs {Haml::Helpers#find\_and\_preserve} on its input.
For example,

    ~ "Foo\n<pre>Bar\nBaz</pre>"

is the same as:

    = find_and_preserve("Foo\n<pre>Bar\nBaz</pre>")

and is compiled to:

    Foo
    <pre>Bar&#x000A;Baz</pre>

See also [Whitespace Preservation](#whitespace_preservation).

### Ruby Interpolation: `#{}`

Ruby code can also be interpolated within plain text using `#{}`,
similarly to Ruby string interpolation.
For example,

    %p This is #{h quality} cake!

is the same as

    %p= "This is the #{h quality} cake!"

and might compile to

    <p>This is scrumptious cake!</p>

Backslashes can be used to escape `#{` strings,
but they don't act as escapes anywhere else in the string.
For example:

    %p
      Look at \\#{h word} lack of backslash: \#{foo}
      And yon presence thereof: \{foo}

might compile to

    <p>
      Look at \yon lack of backslash: #{foo}
      And yon presence thereof: \{foo}
    </p>

Interpolation can also be used within [filters](#filters).
For example:

    :javascript
      $(document).ready(function() {
        alert(#{@message.to_json});
      });

might compile to

    <script type='text/javascript'>
      //<![CDATA[
        $(document).ready(function() {
          alert("Hi there!");
        });
      //]]>
    </script>

### Escaping HTML: `&=` {#escaping_html}

An ampersand followed by one or two equals characters
evaluates Ruby code just like the equals without the ampersand,
but sanitizes any HTML-sensitive characters in the result of the code.
For example:

    &= "I like cheese & crackers"

compiles to

    I like cheese &amp; crackers

If the [`:escape_html`](#escape_html-option) option is set,
`&=` behaves identically to `=`.

`&` can also be used on its own so that `#{}` interpolation is escaped.
For example,

    & I like #{"cheese & crackers"}

compiles to

    I like cheese &amp; crackers

### Unescaping HTML: `!=` {#unescaping_html}

An exclamation mark followed by one or two equals characters
evaluates Ruby code just like the equals would,
but never sanitizes the HTML.

By default, the single equals doesn't sanitize HTML either.
However, if the [`:escape_html`](#escape_html-option) option is set,
`=` will sanitize the HTML, but `!=` still won't.
For example, if `:escape_html` is set:

    = "I feel <strong>!"
    != "I feel <strong>!"

compiles to

    I feel &lt;strong&gt;!
    I feel <strong>!

`!` can also be used on its own so that `#{}` interpolation is unescaped.
For example,

    ! I feel #{"<strong>"}!

compiles to

    I feel <strong>!

## Filters {#filters}

The colon character designates a filter.
This allows you to pass an indented block of text as input
to another filtering program and add the result to the output of Haml.
The syntax is simply a colon followed by the name of the filter.
For example,

    %p
      :markdown
        Textile
        =======

        Hello, *World*

is compiled to

    <p>
      <h1>Textile</h1>

      <p>Hello, <em>World</em></p>
    </p>

Filters can have Ruby code interpolated with `#{}`.
For example,

    - flavor = "raspberry"
    #content
      :textile
        I *really* prefer _#{h flavor}_ jam.

is compiled to

    <div id='content'>
      <p>I <strong>really</strong> prefer <em>raspberry</em> jam.</p>
    </div>

Currently, filters ignore the [`:escape_html`](#escape_html-option) option.
This means that `#{}` interpolation within filters is never HTML-escaped.

Haml has the following filters defined:

{#plain-filter}
### `:plain`
Does not parse the filtered text.
This is useful for large blocks of text without HTML tags,
when you don't want lines starting with `.` or `-` to be parsed.

{#javascript-filter}
### `:javascript`
Surrounds the filtered text with `<script>` and CDATA tags.
Useful for including inline Javascript.

{#css-filter}
### `:css`
Surrounds the filtered text with `<style>` and CDATA tags.
Useful for including inline CSS.

{#cdata-filter}
### `:cdata`
Surrounds the filtered text with CDATA tags.

{#escaped-filter}
### `:escaped`
Works the same as plain, but HTML-escapes the text
before placing it in the document.

{#ruby-filter}
### `:ruby`
Parses the filtered text with the normal Ruby interpreter.
All output sent to `$stdout`, like with `puts`,
is output into the Haml document.
Not available if the [`:suppress_eval`](#suppress_eval-option) option is set to true.
The Ruby code is evaluated in the same context as the Haml template.

{#preserve-filter}
### `:preserve`
Inserts the filtered text into the template with whitespace preserved.
`preserve`d blocks of text aren't indented,
and newlines are replaced with the HTML escape code for newlines,
to preserve nice-looking output.
See also [Whitespace Preservation](#whitespace_preservation).

{#erb-filter}
### `:erb`
Parses the filtered text with ERb, like an RHTML template.
Not available if the [`:suppress_eval`](#suppress_eval-option) option is set to true.
Embedded Ruby code is evaluated in the same context as the Haml template.

{#sass-filter}
### `:sass`
Parses the filtered text with Sass to produce CSS output.

{#textile-filter}
### `:textile`
Parses the filtered text with [Textile](http://www.textism.com/tools/textile).
Only works if [RedCloth](http://redcloth.org) is installed.

{#markdown-filter}
### `:markdown`
Parses the filtered text with [Markdown](http://daringfireball.net/projects/markdown).
Only works if [RDiscount](http://github.com/rtomayko/rdiscount),
[RPeg-Markdown](http://github.com/rtomayko/rpeg-markdown),
[Maruku](http://maruku.rubyforge.org),
or [BlueCloth](www.deveiate.org/projects/BlueCloth) are installed.

{#maruku-filter}
### `:maruku`
Parses the filtered text with [Maruku](http://maruku.rubyforge.org),
which has some non-standard extensions to Markdown.

### Custom Filters

You can also define your own filters. See {Haml::Filters} for details.

## Multiline: `|` {#multiline}

The pipe character designates a multiline string.
It's placed at the end of a line (after some whitespace)
and means that all following lines that end with `|`
will be evaluated as though they were on the same line.
**Note that even the last line in the multiline block
should end wit `|`.**
For example:

    %whoo
      %hoo I think this might get |
        pretty long so I should   |
        probably make it          |
        multiline so it doesn't   |
        look awful.               |
      %p This is short.

is compiled to:

    <whoo>
      <hoo>
        I think this might get pretty long so I should probably make it multiline so it doesn't look awful.
      </hoo>
      <p>This is short</p>
    </whoo>

Using multiline declarations in Haml is intentionally awkward.
This is designed to discourage people from putting lots and lots of Ruby code
in their Haml templates.
If you find yourself using multiline declarations, stop and think:
could I do this better with a helper?

Note that there is one case where it's useful to allow
something to flow over onto multiple lines in a non-awkward manner: attributes.
Some elements just have lots of attributes,
so you can wrap attributes without using `|` (see [Attributes](#attributes)).

## Whitespace Preservation

Sometimes you don't want Haml to indent all your text.
For example, tags like `pre` and `textarea` are whitespace-sensitive;
indenting the text makes them render wrong.

Haml deals with this by "preserving" newlines before they're put into the document --
converting them to the XHTML whitespace escape code, `&#x000A;`.
Then Haml won't try to re-format the indentation.

Literal `textarea` and `pre` tags automatically preserve content given through `=`.
Dynamically-generated `textarea`s and `pre`s can't be preserved automatically,
and so should be passed through {Haml::Helpers#find\_and\_preserve} or the [`~` command](#tilde),
which has the same effect.

Blocks of literal text can be preserved using the [`:preserve` filter](#preserve-filter).

## Helpers

Haml offers a bunch of helpers that are useful
for doing stuff like preserving whitespace,
creating nicely indented output for user-defined helpers,
and other useful things.
The helpers are all documented in the {Haml::Helpers} and {Haml::Helpers::ActionViewExtensions} modules.
