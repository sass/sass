dir = File.dirname(__FILE__)
$LOAD_PATH << dir unless $LOAD_PATH.include?(dir)

# = Sass (Syntactically Awesome StyleSheets)
#
# Sass is a meta-language on top of CSS
# that's used to describe the style of a document
# cleanly and structurally,
# with more power than flat CSS allows.
# Sass both provides a simpler, more elegant syntax for CSS
# and implements various features that are useful
# for creating manageable stylesheets.
#
# == Features
#
# * Whitespace active
# * Well-formatted output
# * Elegant input
# * Feature-rich
#
# == Using Sass
#
# Sass can be used in two ways:
# As a plugin for Ruby on Rails
# and as a standalone parser.
# Sass is bundled with Haml,
# so if the Haml plugin or RubyGem is installed,
# Sass will already be installed as a plugin or gem, respectively.
#
# To install Haml and Sass as a Ruby on Rails plugin,
# use the normal Rails plugin installer:
#
#   ./script/plugin install http://svn.hamptoncatlin.com/haml/tags/stable
#
# Sass templates in Rails don't quite function in the same way as views,
# because they don't contain dynamic content,
# and so only need to be compiled when the template file has been updated.
# By default (see options, below),
# ".sass" files are placed in public/stylesheets/sass.
# Then, whenever necessary, they're compiled into corresponding CSS files in public/stylesheets.
# For instance, public/stylesheets/sass/main.sass would be compiled to public/stylesheets/main.css.
#
# Using Sass in Ruby code is very simple.
# First install the Haml/Sass RubyGem:
#
#   gem install haml
#
# Then you can use it by including the "sass" gem
# and using Sass::Engine like so:
#
#   engine = Sass::Engine.new("#main\n  :background-color #0000ff")
#   engine.render #=> "#main { background-color: #0000ff; }\n"
#
# == CSS Rules
#
# Rules in flat CSS have two elements:
# the selector
# (e.g. "#main", "div p", "li a:hover")
# and the attributes
# (e.g. "color: #00ff00;", "width: 5em;").
#
# Sass has both of these,
# as well as one additional element: nested rules.
#
# === Rules and Selectors
#
# However, some of the syntax is a little different.
# The syntax for selectors is the same,
# but instead of using brackets to delineate the attributes that belong to a particular rule,
# Sass uses two spaces of indentation.
# For example:
#
#   #main p
#     <attribute>
#     <attribute>
#     ...
#
# === Attributes
#
# The syntax for attributes is also slightly different.
# The colon is at the beginning of the attribute,
# rather than between the name and the value,
# so it's easier to tell what elements are attributes just by glancing at them.
# Attributes also don't have semicolons at the end;
# each attribute is on its own line, so they aren't necessary.
# For example:
#
#   #main p
#     :color #00ff00
#     :width 97%
#
# is compiled to:
#
#   #main p {
#     color: #00ff00;
#     width: 97% }
#
# === Nested Rules
#
# Rules can also be nested within each other.
# This signifies that the inner rule's selector is a child of the outer selector.
# For example:
#
#   #main p
#     :color #00ff00
#     :width 97%
#
#     .redbox
#       :background-color #ff0000
#       :color #000000
#
# is compiled to:
#
#   #main p {
#     color: #00ff00;
#     width: 97%; }
#     #main p .redbox {
#       background-color: #ff0000;
#       color: #000000; }
#
# This makes insanely complicated CSS layouts with lots of nested selectors very simple:
#
#   #main
#     :width 97%
#     
#     p, div
#       :font-size 2em
#       a
#         :font-weight bold
#         
#     pre
#       :font-size 3em
#
# is compiled to:
#
#   #main {
#     width: 97%; }
#     #main p, #main div {
#       font-size: 2em; }
#       #main p a, #main div a {
#         font-weight: bold; }
#     #main pre {
#       font-size: 3em; }
#
# === Attribute Namespaces
#
# CSS has quite a few attributes that are in "namespaces;"
# for instance, "font-family," "font-size," and "font-weight"
# are all in the "font" namespace.
# In CSS, if you want to set a bunch of attributes in the same namespace,
# you have to type it out each time.
# Sass offers a shortcut for this:
# just write the namespace one,
# then indent each of the sub-attributes within it.
# For example:
#
#   .funky
#     :font
#       :family fantasy
#       :size 30em
#       :weight bold
#
# is compiled to:
#
#   .funky {
#     font-family: fantasy;
#     font-size: 30em;
#     font-weight: bold; }
#
module Sass; end

require 'sass/engine'
