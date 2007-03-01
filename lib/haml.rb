dir = File.dirname(__FILE__)
$LOAD_PATH << dir unless $LOAD_PATH.include?(dir)

# = Haml (XHTML Abstraction Markup Language)
# 
# Haml is a markup language
# that's used to cleanly and simply describe the XHTML of any web document,
# without the use of inline code.
# Haml functions as a replacement
# for inline page templating systems such as PHP, ERB, and ASP. 
# However, Haml avoids the need for explicitly coding XHTML into the template, 
# because it is actually an abstract description of the XHTML,
# with some code to generate dynamic content.
# 
# == Features
# 
# * Whitespace active
# * Well-formatted markup
# * DRY
# * Follows CSS conventions
# * Integrates Ruby code
# * Implements Rails templates with the .haml extension
#
# == Using Haml
#
# Haml can be used in two ways:
# as a plugin for Ruby on Rails,
# and as a standalong Ruby module.
#
# === Rails
# 
# Haml is most commonly used as a plugin.
# It can be installed as a plugin using the Rails plugin installer:
# 
#   ./script/plugin install http://svn.hamptoncatlin.com/haml/tags/stable
#
# Once it's installed, all view files with the ".haml" extension
# will be compiled using Haml.
#
# You can access instance variables in Haml templates
# the same way you do in ERb templates.
# Helper methods are also available in Haml templates.
# For example:
# 
#   # file: app/controllers/movies_controller.rb
# 
#   class MoviesController < ApplicationController
#     def index
#       @title = "Teen Wolf"
#     end
#   end
# 
#   # file: app/views/movies/index.haml
# 
#   #content
#    .title
#      %h1= @title
#      = link_to 'Home', home_url
# 
# may be compiled to:
# 
#   <div id='content'>
#     <div class='title'>
#       <h1>Teen Wolf</h1>
#       <a href='/'>Home</a>
#     </div>
#   </div>
#
# === Ruby Module
#
# Haml can also be used completely separately from Rails and ActionView.
# To do this, install the gem with RubyGems:
#
#   gem install haml
#
# You can then use it by including the "haml" gem in Ruby code,
# and using Haml::Engine like so:
#
#   engine = Haml::Engine.new("%p Haml code!")
#   engine.render #=> "<p>Haml code!</p>\n"
# 
# == Characters with meaning to Haml
# 
# Various characters, when placed at a certain point in a line,
# instruct Haml to render different types of things.
# 
# === XHTML Tags
# 
# These characters render XHTML tags.
# 
# ==== %
# 
# 
# The percent character is placed at the beginning of a line.
# It's followed immediately by the name of an element,
# then optionally by modifiers (see below), a space,
# and text to be rendered inside the element.
# It creates an element in the form of <tt><element></element></tt>.
# For example:
# 
#   %one
#     %two
#       %three Hey there
# 
# is compiled to:
# 
#   <one>
#     <two>
#       <three>Hey there</three>
#     </two>
#   </one>
# 
# Any string is a valid element name;
# Haml will automatically generate opening and closing tags for any element.
# 
# ==== {}
# 
# Brackets represent a Ruby hash
# that is used for specifying the attributes of an element.
# It is literally evaluated as a Ruby hash,
# so logic will work in it and local variables may be used.
# Quote characters within the attribute
# will be replaced by appropriate escape sequences.
# The hash is placed after the tag is defined.
# For example:
# 
#   %head{ :name => "doc_head" }
#     %script{ 'type' => "text/" + "javascript",
#              :src   => "javascripts/script_#{2 + 7}" }
# 
# is compiled to:
# 
#   <head name="doc_head">
#     <script src='javascripts/script_9' type='text/javascript'>
#     </script>
#   </head>
# 
# ==== []
# 
# Square brackets follow a tag definition and contain a Ruby object
# that is used to set the class and id of that tag.
# The class is set to the object's class
# (transformed to use underlines rather than camel case)
# and the id is set to the object's class, followed by its id.
# Because the id of an object is normally an obscure implementation detail,
# this is most useful for elements that represent instances of Models.
# For example:
# 
#   # file: app/controllers/users_controller.rb
# 
#   def show
#     @user = CrazyUser.find(15)
#   end
# 
#   # file: app/views/users/show.haml
# 
#   %div[@user]
#     %bar[290]/
#     Hello!
# 
# is compiled to:
# 
#   <div class="crazy_user" id="crazy_user_15">
#     <bar class="fixnum" id="fixnum_581" />
#     Hello!
#   </div>
# 
# This is based off of DHH's SimplyHelpful syntax,
# as presented at RailsConf Europe 2006.
# 
# ==== /
# 
# The forward slash character, when placed at the end of a tag definition,
# causes the tag to be self-closed.
# For example:
# 
#   %br/
#   %meta{'http-equiv' => 'Content-Type', :content => 'text/html'}/
# 
# is compiled to:
# 
#   <br />
#   <meta http-equiv='Content-Type' content='text/html' />
# 
# ==== . and #
# 
# The period and pound sign are borrowed from CSS.
# They are used as shortcuts to specify the <tt>class</tt>
# and <tt>id</tt> attributes of an element, respectively.
# Multiple class names can be specified in a similar way to CSS,
# by chaining the class names together with periods.
# They are placed immediately after the tag and before an attributes hash.
# For example:
# 
#   %div#things
#     %span#rice Chicken Fried
#     %p.beans{ :food => 'true' } The magical fruit
#     %h1.class.otherclass#id La La La
# 
# is compiled to:
# 
#   <div id='things'>
#     <span id='rice'>Chicken Fried</span>
#     <p class='beans' food='true'>The magical fruit</p>
#     <h1 class='class otherclass' id='id'>La La La</h1>
#   </div>
# 
# And,
# 
#   #content
#     .articles
#       .article.title
#         Doogie Howser Comes Out
#       .article.date
#         2006-11-05
#       .article.entry
#         Neil Patrick Harris would like to dispel any rumors that he is straight
# 
# is compiled to:
# 
#   <div id="content">
#     <div class="articles">
#       <div class="article title">Doogie Howser Comes Out</div>
#       <div class="article date">2006-11-05</div>
#       <div class="article entry">
#         Neil Patrick Harris would like to dispel any rumors that he is straight
#       </div>
#     </div>
#   </div>
# 
# ==== Implicit Div Elements
# 
# Because the div element is used so often, it is the default element.
# If you only define a class and/or id using the <tt>.</tt> or <tt>#</tt> syntax,
# a div element is automatically used.
# For example:
# 
#   #collection
#     .item
#       .description What a cool item!
# 
# is the same as:
# 
#   %div{:id => collection}
#     %div{:class => 'item'}
#       %div{:class => 'description'} What a cool item!
# 
# and is compiled to:
# 
#   <div id='collection'>
#     <div class='item'>Broken record album</div>
#     <div class='description'>What a cool item!</div>
#   </div>
# 
# ==== =
# 
# <tt>=</tt> is placed at the end of a tag definition,
# after class, id, and attribute declarations.
# It's just a shortcut for inserting Ruby code into an element.
# It works the same as <tt>=</tt> without a tag:
# it inserts the result of the Ruby code into the template.
# However, if the result is short enough,
# it is displayed entirely on one line.
# For example:
# 
#   %p= "hello"
# 
# is not quite the same as:
# 
#   %p
#     = "hello"
# 
# It's compiled to:
# 
#   <p>hello</p>
# 
# === XHTML Helpers
# 
# ==== No Special Character
# 
# If no special character appears at the beginning of a line,
# the line is rendered as plain text.
# For example:
# 
#   %gee
#     %whiz
#       Wow this is cool!
# 
# is compiled to:
# 
#   <gee>
#     <whiz>
#       Wow this is cool!
#     </whiz>
#   </gee>
# 
# ==== !!!
# 
# When describing XHTML documents with Haml,
# you can have a document type or XML prolog generated automatically
# by including the characters <tt>!!!</tt>.
# For example:
# 
#   !!! XML
#   !!!
#   %html
#     %head
#       %title Myspace
#     %body
#       %h1 I am the international space station
#       %p Sign my guestbook
# 
# is compiled to:
# 
#   <?xml version="1.0" encoding="utf-8" ?>
#   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
#   <html>
#     <head>
#       <title>Myspace</title>
#     </head>
#     <body>
#       <h1>I am the international space station</h1>
#       <p>Sign my guestbook</p>
#     </body>
#   </html>
# 
# You can also specify the version and type of XHTML after the <tt>!!!</tt>.
# XHTML 1.0 Strict, Transitional, and Frameset and XHTML 1.1 are supported.
# The default version is 1.0 and the default type is Transitional.
# For example:
# 
#   !!! 1.1
# 
# is compiled to:
# 
#   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
# 
# and
# 
#   !!! Strict
# 
# is compiled to:
# 
#   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
# 
# If you're not using the UTF-8 characterset for your document,
# you can specify which encoding should appear
# in the XML prolog in a similar way.
# For example:
# 
#   !!! XML iso-8859-1
# 
# is compiled to:
# 
#   <?xml version="1.0" encoding="iso-8859-1" ?>
# 
# ==== /
# 
# The forward slash character, when placed at the beginning of a line,
# wraps all text after it in an HTML comment.
# For example:
# 
#   %billabong
#     / This is the billabong element
#     I like billabongs!
# 
# is compiled to:
# 
#   <billabong>
#     <!-- This is the billabong element -->
#     I like billabongs!
#   </billabong>
# 
# The forward slash can also wrap indented sections of code. For example:
# 
#   /
#     %p This doesn't render...
#     %div
#       %h1 Because it's commented out!
# 
# is compiled to:
# 
#   <!--
#     <p>This doesn't render...</p>
#     <div>
#       <h1>Because it's commented out!</h1>
#     </div>
#   -->
# 
# You can also use Internet Explorer conditional comments
# (about)[http://www.quirksmode.org/css/condcom.html]
# by enclosing the condition in square brackets after the <tt>/</tt>.
# For example:
# 
#   /[if IE]
#     %a{ :href => 'http://www.mozilla.com/en-US/firefox/' }
#       %h1 Get Firefox
# 
# is compiled to:
# 
#   <!--[if IE]>
#     <a href='http://www.mozilla.com/en-US/firefox/'>
#       <h1>Get Firefox</h1>
#     </a>
#   <![endif]-->
# 
# ==== \
# 
# The backslash character escapes the first character of a line,
# allowing use of otherwise interpreted characters as plain text.
# For example:
# 
#   %title
#     = @title
#     \- MySite
# 
# is compiled to:
# 
#   <title>
#     MyPage
#     - MySite
#   </title>
# 
# ==== |
# 
# The pipe character designates a multiline string.
# It's placed at the end of a line
# and means that all following lines that end with <tt>|</tt>
# will be evaluated as though they were on the same line.
# For example:
# 
#   %whoo
#     %hoo I think this might get |
#       pretty long so I should |
#       probably make it |
#       multiline so it doesn't |
#       look awful. |
#     %p This is short.
# 
# is compiled to:
# 
#   <whoo>
#     <hoo>
#       I think this might get pretty long so I should probably make it multiline so it doesn't look awful.
#     </hoo>
#   </whoo>
#
# ==== :
#
# The colon character designates a filter.
# This allows you to pass an indented block of text as input
# to another filtering program and add the result to the output of Haml.
# The syntax is simply a colon followed by the name of the filter.
# For example,
#
#   %p
#     :markdown
#       Textile
#       =======
#
#       Hello, *World*
#
# is compiled to
#
#   <p>
#     <h1>Textile</h1>
#
#     <p>Hello, <em>World</em></p>
#   </p>
#
# Haml has the following filters defined:
#
# [plain]     Does not parse the filtered text.
#             This is useful for large blocks of text without HTML tags,
#             when you don't want lines starting with <tt>.</tt> or <tt>-</tt>
#             to be parsed.
#
# [ruby]      Parses the filtered text with the normal Ruby interpreter.
#             All output sent to <tt>$stdout</tt>, like with +puts+,
#             is output into the Haml document.
#             Not available if the <tt>suppress_eval</tt> option is set to true.
#
# [preserve]  Inserts the filtered text into the template with whitespace preserved.
#             <tt>preserve</tt>d blocks of text aren't indented,
#             and newlines are replaced with the HTML escape code for newlines,
#             to preserve nice-looking output.
#
# [erb]       Parses the filtered text with ERB, like an RHTML template.
#             Not available if the <tt>suppress_eval</tt> option is set to true.
#             At the moment, this doesn't support access to variables
#             defined by Ruby on Rails or Haml code.
#
# [sass]      Parses the filtered text with Sass to produce CSS output.
#
# [redcloth]  Parses the filtered text with RedCloth (http://whytheluckystiff.net/ruby/redcloth),
#             which uses both Textile and Markdown syntax.
#             Only works if RedCloth is installed.
#
# [textile]   Parses the filtered text with Textile (http://www.textism.com/tools/textile).
#             Only works if RedCloth is installed.
#
# [markdown]  Parses the filtered text with Markdown (http://daringfireball.net/projects/markdown).
#             Only works if RedCloth or BlueCloth (http://www.deveiate.org/projects/BlueCloth)
#             is installed
#             (BlueCloth takes precedence if both are installed).
#
# You can also define your own filters (see Setting Options, below).
# 
# === Ruby evaluators
# 
# ==== =
# 
# The equals character is followed by Ruby code,
# which is evaluated and the output inserted into the document as plain text.
# For example:
# 
#   %p
#     = ['hi', 'there', 'reader!'].join " "
#     = "yo"
# 
# is compiled to:
# 
#   <p>
#     hi there reader!
#     yo
#   </p>
# 
# ==== -
# 
# The hyphen character makes the text following it into "silent script":
# Ruby script that is evaluated, but not output.
# 
# <b>It is not recommended that you use this widely;
# almost all processing code and logic should be restricted
# to the Controller, the Helper, or partials.</b>
# 
# For example:
# 
#   - foo = "hello"
#   - foo << " there"
#   - foo << " you!"
#   %p= foo
# 
# is compiled to:
# 
#   <p>
#     hello there you!
#   </p>
# 
# ===== Blocks
# 
# Ruby blocks, like XHTML tags, don't need to be explicitly closed in Haml.
# Rather, they're automatically closed, based on indentation.
# A block begins whenever the indentation is increased
# after a silent script command.
# It ends when the indentation decreases
# (as long as it's not an +else+ clause or something similar).
# For example:
# 
#   - (42...47).each do |i|
#     %p= i
#   %p See, I can count!
# 
# is compiled to:
# 
#   <p>
#     42
#   </p>
#   <p>
#     43
#   </p>
#   <p>
#     44
#   </p>
#   <p>
#     45
#   </p>
#   <p>
#     46
#   </p>
# 
# Another example:
# 
#   %p
#     - case 2
#     - when 1
#       = "1!"
#     - when 2
#       = "2?"
#     - when 3
#       = "3."
# 
# is compiled to:
# 
#   <p>
#     2?
#   </p>
# 
# == Haml Options
# 
# Options can be set by setting the hash <tt>Haml::Template.options</tt>
# from <tt>environment.rb</tt> in Rails,
# or by passing an options hash to Haml::Engine.
# Available options are:
# 
# [<tt>:suppress_eval</tt>] Whether or not attribute hashes and Ruby scripts
#                           designated by <tt>=</tt> or <tt>~</tt> should be
#                           evaluated. If this is true, said scripts are
#                           rendered as empty strings. Defaults to false.
# 
# [<tt>:precompiled</tt>]   A string containing a precompiled Haml template.
#                           If this is passed, <tt>template</tt> is ignored
#                           and no precompilation is done.
# 
# [<tt>:attr_wrapper</tt>]  The character that should wrap element attributes.
#                           This defaults to <tt>'</tt> (an apostrophe). Characters
#                           of this type within the attributes will be escaped
#                           (e.g. by replacing them with <tt>&apos;</tt>) if
#                           the character is an apostrophe or a quotation mark.
#
# [<tt>:filename</tt>]      The name of the Haml file being parsed.
#                           This is only used as information when exceptions are raised.
#                           This is automatically assigned when working through ActionView,
#                           so it's really only useful for the user to assign
#                           when dealing with Haml programatically.
#
# [<tt>:filters</tt>]       A hash of filters that can be applied to Haml code.
#                           The keys are the string names of the filters;
#                           the values are references to the classes of the filters.
#                           User-defined filters should always have lowercase keys,
#                           and should have:
#                           * An +initialize+ method that accepts one parameter,
#                             the text to be filtered.
#                           * A +render+ method that returns the result of the filtering.
# 
# [<tt>:locals</tt>]        The local variables that will be available within the
#                           template. For instance, if <tt>:locals</tt> is
#                           <tt>{ :foo => "bar" }</tt>, then within the template,
#                           <tt>= foo</tt> will produce <tt>bar</tt>.
#
module Haml; end

require 'haml/engine'
