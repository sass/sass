# Haml and Sass

Haml and Sass are templating engines
for the two most common types of documents on the web:
HTML and CSS, respectively.
They are designed to make it both easier and more pleasant
to code HTML and CSS documents,
by eliminating redundancy,
reflecting the underlying structure that the document represents,
and providing elegant, easily understandable, and powerful syntax.

## Using

Haml and Sass can be used from the command line
or as part of a Ruby web framework.
The first step is to install the gem:

    gem install haml

After you convert some HTML to Haml or some CSS to Sass,
you can run

    haml document.haml
    sass style.sass

to compile them.
For more information on these commands, check out

    haml --help
    sass --help

To install Haml and Sass as a Rails plugin,
just run `haml --rails path/to/rails/app`
and both Haml and Sass will be installed.
Views with the `.html.haml` extension will automatically use Haml.
Sass is a little more complicated;
`.sass` files should be placed in `public/stylesheets/sass`,
where they'll be automatically compiled
to corresponding CSS files in `public/stylesheets` when needed
(the Sass template directory is customizable...
see [the Sass reference](http://sass-lang.com/docs/yardoc/SASS_REFERENCE.md.html#template_location-option) for details).

For Merb, `.html.haml` views will work without any further modification.
To enable Sass, you also need to add a dependency.
To do so, just add

    dependency "merb-haml"

to `config/dependencies.rb` (or `config/init.rb` in a flat/very flat Merb application).
Then it'll work just like it does in Rails.

Sass can also be used with any Rack-enabled web framework.
To do so, just add

    require 'sass/plugin/rack'
    use Sass::Plugin::Rack

to `config.ru`.
Then any Sass files in `public/stylesheets/sass`
will be compiled CSS files in `public/stylesheets` on every request.

To use Haml and Sass programatically,
check out the [YARD documentation](http://haml-lang.com/docs/yardoc).

## Formatting

### Haml

The most basic element of Haml
is a shorthand for creating HTML:

    %tagname{:attr1 => 'value1', :attr2 => 'value2'} Contents

No end-tag is needed; Haml handles that automatically.
If you prefer HTML-style attributes, you can also use:

    %tagname(attr1='value1' attr2='value2') Contents

Adding `class` and `id` attributes is even easier.
Haml uses the same syntax as the CSS that styles the document:

    %tagname#id.class

In fact, when you're using the `<div>` tag,
it becomes _even easier_.
Because `<div>` is such a common element,
a tag without a name defaults to a div. So

    #foo Hello!

becomes

    <div id='foo'>Hello!</div>

Haml uses indentation
to bring the individual elements to represent the HTML structure.
A tag's children are indented beneath than the parent tag.
Again, a closing tag is automatically added.
For example:

    %ul
      %li Salt
      %li Pepper

becomes:

    <ul>
      <li>Salt</li>
      <li>Pepper</li>
    </ul>

You can also put plain text as a child of an element:

    %p
      Hello,
      World!

It's also possible to embed Ruby code into Haml documents.
An equals sign, `=`, will output the result of the code.
A hyphen, `-`, will run the code but not output the result.
You can even use control statements
like `if` and `while`:

    %p
      Date/Time:
      - now = DateTime.now
      %strong= now
      - if now > DateTime.parse("December 31, 2006")
        = "Happy new " + "year!"

Haml provides far more tools than those presented here.
Check out the reference documentation in the Haml module.

### Sass

At its most basic,
Sass is just another way of writing CSS.
Although it's very much like normal CSS,
the basic syntax offers a few helpful features:
indentation indicates the properties in a rule,
rather than non-DRY brackets;
and newlines indicate the end of a properties,
rather than a semicolon.
For example:

    #main
      background-color: #f00
      width: 98%

becomes:

    #main {
      background-color: #f00;
      width: 98% }

However, Sass provides much more than a way to make CSS look nice.
In CSS, it's important to have accurate selectors,
so your styles don't just apply to everything.
However, in order to do this,
you need to use nested element selectors.
These get very ugly very quickly.
I'm sure everyone's had to write something like
"#main .sidebar .top p h1 a",
followed by
"#main .sidebar .top p h1 a:visited" and
"#main .sidebar .top p h1 a:hover".
Well, Sass gets rid of that.
Like Haml, it uses indentation to indicate the structure of the document.
So, what was:

    #main {
      width: 90%;
    }
    #main p {
      border-style: solid;
      border-width: 1px;
      border-color: #00f;
    }
    #main p a {
      text-decoration: none;
      font-weight: bold;
    }
    #main p a:hover {
      text-decoration: underline;
    }

becomes:

    #main
      width: 90%
      p
        border-style: solid
        border-width: 1px
        border-color: #00f
        a
          text-decoration: none
          font-weight: bold
        a:hover
          text-decoration: underline

Pretty nice, no? Well, it gets better.
One of the main complaints against CSS is that it doesn't allow variables.
What if have a color or a width you re-use all the time?
In CSS, you just have to re-type it each time,
which is a nightmare when you decide to change it later.
Not so for Sass!
You can use the `!` character to set variables.
Then, if you put `=` after your property name,
you can set it to a variable.
For example:

    !note_bg= #55aaff

    #main
      width: 70%
      .note
        background-color = !note_bg
      p
        width: 5em
        background-color = !note_bg

becomes:

    #main {
      width: 70%; }
      #main .note {
        background-color: #55aaff; }
      #main p {
        width: 5em;
        background-color: #55aaff; }

You can even do simple arithmetic operations with variables,
adding numbers and even colors together:

    !main_bg= #46ar12
    !main_width= 40em

    #main
      background-color = !main_bg
      width = !main_width
      .sidebar
        background-color = !main_bg + #333333
        width = !main_width - 25em

becomes:

    #main {
      background-color: #46a312;
      width: 40em; }
      #main .sidebar {
        background-color: #79d645;
        width: 15em; }

Taking the idea of variables a bit further are mixins.
These let you group whole bunches of CSS properties into a single
directive and then include those anywhere you want:

    =blue-border
      border:
        color: blue
        width: 2px
        style: dotted

    .comment
      +blue-border
      padding: 2px
      margin: 10px 0

    .reply
      +blue-border

becomes:

    .comment {
      border-color: blue;
      border-width: 2px;
      border-style: dotted;
      padding: 2px;
      margin: 10px 0;
    }

    .reply {
      border-color: blue;
      border-width: 2px;
      border-style: dotted;
    }

A comprehensive list of features is in
the documentation for the Sass module.

## Indentation

Indentation can be made up of one or more tabs or spaces.
However, indentation must be consistent within a given document.
Hard tabs and spaces can't be mixed,
and the same number of tabs or spaces must be used throughout.

## Executables

The Haml gem includes several executables that are useful
for dealing with Haml and Sass from the command line.

### `haml`

The `haml` executable transforms a source Haml file into HTML.
See `haml --help` for further information and options.

### `sass`

The `sass` executable transforms a source Sass file into CSS.
See `sass --help` for further information and options.

### `html2haml`

The `html2haml` executable attempts to transform HTML,
optionally with ERB markup, into Haml code.
Since HTML is so variable, this transformation is not always perfect;
it's a good idea to have a human check the output of this tool.
See `html2haml --help` for further information and options.

### `css2sass`

The `css2sass` executable attempts to transform CSS into Sass code.
This transformation attempts to use Sass nesting where possible.
See `css2sass --help` for further information and options.

## Authors

Haml and Sass were created by [Hampton Catlin](http://hamptoncatlin.com)
(hcatlin) and he is the author of the original implementation. However, Hampton
doesn't even know his way around the code anymore and now occasionally consults
on the language issues.  Hampton lives in Jacksonville, Florida and is the lead
mobile developer for Wikimedia.

[Nathan Weizenbaum](http://nex-3.com) is the primary developer and architect of
the "modern" Ruby implementation of Haml. His hard work has kept the project
alive by endlessly answering forum posts, fixing bugs, refactoring, finding
speed improvements, writing documentation, implementing new features, and
getting Hampton coffee (a fitting task for a boy-genius). Nathan lives in
Seattle, Washington and while not being a student at the University of
Washington or working at an internship, he consults for Unspace Interactive.

[Chris Eppstein](http://acts-as-architect.blogspot.com) is a core contributor to
Sass and the creator of Compass, the first Sass-based framework. Chris focuses
on making Sass more powerful, easy to use, and on ways to speed its adoption
through the web development community. Chris lives in San Jose, California with
his wife and daughter. He is the Software Architect for
[Caring.com](http://caring.com), a website devoted to the 34 Million caregivers
whose parents are sick or elderly, that uses Haml and Sass.

If you use this software, you must pay Hampton a compliment. And
buy Nathan some jelly beans. Maybe pet a kitten. Yeah. Pet that kitty.

Some of the work on Haml was supported by Unspace Interactive.

Beyond that, the implementation is licensed under the MIT License.
Okay, fine, I guess that means compliments aren't __required__.
