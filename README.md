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
    sass style.scss

to compile them.
For more information on these commands, check out

    haml --help
    sass --help

To install Haml and Sass in Rails 2,
just add `config.gem "haml"` to `config/environment.rb`.
In Rails 3, add `gem "haml"` to your Gemfile instead.
and both Haml and Sass will be installed.
Views with the `.html.haml` extension will automatically use Haml.
Sass is a little more complicated;
`.sass` files should be placed in `public/stylesheets/sass`,
where they'll be automatically compiled
to corresponding CSS files in `public/stylesheets` when needed
(the Sass template directory is customizable...
see [the Sass reference](http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#template_location-option) for details).

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
check out the [YARD documentation](http://haml-lang.com/docs/yardoc/).

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
Check out the [reference documentation](http://beta.haml-lang.com/docs/yardoc/file.HAML_REFERENCE.html)
for full details.

#### Indentation

Haml's indentation can be made up of one or more tabs or spaces.
However, indentation must be consistent within a given document.
Hard tabs and spaces can't be mixed,
and the same number of tabs or spaces must be used throughout.

### Sass

Sass is an extension of CSS
that adds power and elegance to the basic language.
It allows you to use [variables][vars], [nested rules][nested],
[mixins][mixins], [inline imports][imports],
and more, all with a fully CSS-compatible syntax.
Sass helps keep large stylesheets well-organized,
and get small stylesheets up and running quickly,
particularly with the help of
[the Compass style library](http://compass-style.org).

[vars]:    http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#variables_
[nested]:  http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#nested_rules_
[mixins]:  http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#mixins
[imports]: http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#import

Sass has two syntaxes.
The one presented here, known as "SCSS" (for "Sassy CSS"),
is fully CSS-compatible.
The other (older) syntax, known as the indented syntax or just "Sass",
is whitespace-sensitive and indentation-based.
For more information, see the [reference documentation][syntax].

[syntax]: http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#syntax

To run the following examples and see the CSS they produce,
put them in a file called `test.scss` and run `sass test.scss`.

#### Nesting

Sass avoids repetition by nesting selectors within one another.
The same thing works for properties.

    table.hl {
      margin: 2em 0;
      td.ln { text-align: right; }
    }

    li {
      font: {
        family: serif;
        weight: bold;
        size: 1.2em;
      }
    }

#### Variables

Use the same color all over the place?
Need to do some math with height and width and text size?
Sass supports variables, math operations, and many useful functions.

    $blue: #3bbfce;
    $margin: 16px;

    .content_navigation {
      border-color: $blue;
      color: darken($blue, 10%);
    }

    .border {
      padding: $margin / 2;
      margin: $margin / 2;
      border-color: $blue;
    }

#### Mixins

Even more powerful than variables,
mixins allow you to re-use whole chunks of CSS,
properties or selectors.
You can even give them arguments. 

    @mixin table-scaffolding {
      th {
        text-align: center;
        font-weight: bold;
      }
      td, th { padding: 2px; }
    }

    @mixin left($dist) {
      float: left;
      margin-left: $dist;
    }

    #data {
      @include left(10px);
      @include table-scaffolding;
    }

A comprehensive list of features is available
in the [Sass reference](http://beta.sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html).

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

### `sass-convert`

The `sass-convert` executable converts between CSS, Sass, and SCSS.
When converting from CSS to Sass or SCSS,
nesting is applied where appropriate.
See `sass-convert --help` for further information and options.

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
