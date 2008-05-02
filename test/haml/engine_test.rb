#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/test_helper'

class EngineTest < Test::Unit::TestCase
  # A map of erroneous Sass documents to the error messages they should produce.
  # The error messages may be arrays;
  # if so, the second element should be the line number that should be reported for the error.
  # If this isn't provided, the tests will assume the line number should be the last line of the document.
  EXCEPTION_MAP = {
    "!!!\n  a" => "Illegal nesting: nesting within a header command is illegal.",
    "a\n  b" => "Illegal nesting: nesting within plain text is illegal.",
    "/ a\n  b" => "Illegal nesting: nesting within a tag that already has content is illegal.",
    "% a" => 'Invalid tag: "% a".',
    "%p a\n  b" => "Illegal nesting: content can't be both given on the same line as %p and nested within it.",
    "%p=" => "There's no Ruby code for = to evaluate.",
    "%p~" => "There's no Ruby code for ~ to evaluate.",
    "~" => "There's no Ruby code for ~ to evaluate.",
    "=" => "There's no Ruby code for = to evaluate.",
    "%p/\n  a" => "Illegal nesting: nesting within a self-closing tag is illegal.",
    "%p\n\ta" => <<END.strip,
A tab character was used for indentation. Haml must be indented using two spaces.
Are you sure you have soft tabs enabled in your editor?
END
    "%p\n a" => "1 space was used for indentation. Haml must be indented using two spaces.",
    "%p\n   a" => "3 spaces were used for indentation. Haml must be indented using two spaces.",
    "%p\n    a" => "4 spaces were used for indentation. Haml must be indented using two spaces.",
    ":a\n  b" => ['Filter "a" is not defined.', 1],
    ":a= b" => 'Invalid filter name ":a= b".',
    "." => "Illegal element: classes and ids must have values.",
    ".#" => "Illegal element: classes and ids must have values.",
    ".{} a" => "Illegal element: classes and ids must have values.",
    ".= a" => "Illegal element: classes and ids must have values.",
    "%p..a" => "Illegal element: classes and ids must have values.",
    "%a/ b" => "Self-closing tags can't have content.",

    # Regression tests
    "- raise 'foo'\n\n\n\nbar" => ["foo", 1],
    "= 'foo'\n-raise 'foo'" => ["foo", 2],
    "\n\n\n- raise 'foo'" => ["foo", 4],
  }

  User = Struct.new('User', :id)

  def render(text, options = {}, &block)
    scope  = options.delete(:scope)  || Object.new
    locals = options.delete(:locals) || {}
    Haml::Engine.new(text, options).to_html(scope, locals, &block)
  end

  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  def test_attributes_should_render_correctly
    assert_equal("<div class='atlantis' style='ugly'>\n</div>", render(".atlantis{:style => 'ugly'}").chomp)
  end

  def test_ruby_code_should_work_inside_attributes
    author = 'hcatlin'
    assert_equal("<p class='3'>foo</p>", render("%p{:class => 1+2} foo").chomp)
  end

  def test_nil_should_render_empty_tag
    assert_equal("<div class='no_attributes'>\n</div>",
                 render(".no_attributes{:nil => nil}").chomp)
  end

  def test_strings_should_get_stripped_inside_tags
    assert_equal("<div class='stripped'>This should have no spaces in front of it</div>",
                 render(".stripped    This should have no spaces in front of it").chomp)
  end

  def test_one_liner_should_be_one_line
    assert_equal("<p>Hello</p>", render('%p Hello').chomp)
  end

  def test_one_liner_with_newline_shouldnt_be_one_line
    assert_equal("<p>\n  foo\n  bar\n</p>", render('%p= "foo\nbar"').chomp)
  end

  def test_multi_render
    engine = Haml::Engine.new("%strong Hi there!")
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
  end

  def test_double_equals
    assert_equal("<p>Hello World</p>\n", render('%p== Hello #{who}', :locals => {:who => 'World'}))
    assert_equal("<p>\n  Hello World\n</p>\n", render("%p\n  == Hello \#{who}", :locals => {:who => 'World'}))
  end

  def test_double_equals_in_the_middle_of_a_string
    assert_equal("\"title 'Title'. \"\n",
                 render("== \"title '\#{\"Title\"}'. \""))
  end

  def test_nil_tag_value_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= nil"))
  end

  def test_tag_with_failed_if_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= 'Hello' if false"))
  end

  def test_static_attributes_with_empty_attr
    assert_equal("<img alt='' src='/foo.png' />\n", render("%img{:src => '/foo.png', :alt => ''}"))
  end

  def test_dynamic_attributes_with_empty_attr
    assert_equal("<img alt='' src='/foo.png' />\n", render("%img{:width => nil, :src => '/foo.png', :alt => String.new}"))
  end

  def test_end_of_file_multiline
    assert_equal("<p>0</p>\n<p>1</p>\n<p>2</p>\n", render("- for i in (0...3)\n  %p= |\n   i |"))
  end

  def test_cr_newline
    assert_equal("<p>foo</p>\n<p>bar</p>\n<p>baz</p>\n<p>boom</p>\n", render("%p foo\r%p bar\r\n%p baz\n\r%p boom"))
  end

  def test_textareas
    assert_equal("<textarea>Foo&#x000A;  bar&#x000A;   baz</textarea>\n",
                 render('%textarea= "Foo\n  bar\n   baz"'))

    assert_equal("<pre>Foo&#x000A;  bar&#x000A;   baz</pre>\n",
                 render('%pre= "Foo\n  bar\n   baz"'))

    assert_equal("<textarea>#{'a' * 100}</textarea>\n",
                 render("%textarea #{'a' * 100}"))
  end

  def test_boolean_attributes
    assert_equal("<p bar baz='true' foo='bar'>\n</p>\n",
                 render("%p{:foo => 'bar', :bar => true, :baz => 'true'}", :format => :html4))
    assert_equal("<p bar='bar' baz='true' foo='bar'>\n</p>\n",
                 render("%p{:foo => 'bar', :bar => true, :baz => 'true'}", :format => :xhtml))

    assert_equal("<p baz='false' foo='bar'>\n</p>\n",
                 render("%p{:foo => 'bar', :bar => false, :baz => 'false'}", :format => :html4))
    assert_equal("<p baz='false' foo='bar'>\n</p>\n",
                 render("%p{:foo => 'bar', :bar => false, :baz => 'false'}", :format => :xhtml))
  end

  # HTML escaping tests

  def test_ampersand_equals_should_escape
    assert_equal("<p>\n  foo &amp; bar\n</p>\n", render("%p\n  &= 'foo & bar'", :escape_html => false))
  end

  def test_ampersand_equals_inline_should_escape
    assert_equal("<p>foo &amp; bar</p>\n", render("%p&= 'foo & bar'", :escape_html => false))
  end

  def test_bang_equals_should_not_escape
    assert_equal("<p>\n  foo & bar\n</p>\n", render("%p\n  != 'foo & bar'", :escape_html => true))
  end

  def test_bang_equals_inline_should_not_escape
    assert_equal("<p>foo & bar</p>\n", render("%p!= 'foo & bar'", :escape_html => true))
  end
  
  def test_static_attributes_should_be_escaped
    assert_equal("<img class='atlantis' style='ugly&amp;stupid' />\n",
                 render("%img.atlantis{:style => 'ugly&stupid'}", :escape_html => true))
    assert_equal("<div class='atlantis' style='ugly&amp;stupid'>foo</div>\n",
                 render(".atlantis{:style => 'ugly&stupid'} foo", :escape_html => true))
    assert_equal("<p class='atlantis' style='ugly&amp;stupid'>foo</p>\n",
                render("%p.atlantis{:style => 'ugly&stupid'}= 'foo'", :escape_html => true))
  end

  def test_dynamic_attributes_should_be_escaped
    assert_equal("<img alt='' src='/foo.png' />\n",
                 render("%img{:width => nil, :src => '/foo.png', :alt => String.new}", :escape_html => true))
    assert_equal("<p alt='' src='/foo.png'>foo</p>\n",
                 render("%p{:width => nil, :src => '/foo.png', :alt => String.new} foo", :escape_html => true))
    assert_equal("<div alt='' src='/foo.png'>foo</div>\n",
                 render("%div{:width => nil, :src => '/foo.png', :alt => String.new}= 'foo'", :escape_html => true))
  end
  
  def test_string_interpolation_should_be_esaped
    assert_equal("<p>4&amp;3</p>\n", render("%p== #{2+2}&#{2+1}", :escape_html => true))
    assert_equal("<p>4&3</p>\n", render("%p== #{2+2}&#{2+1}", :escape_html => false))
  end

  def test_escaped_inline_string_interpolation
    assert_equal("<p>4&amp;3</p>\n", render("%p&== #{2+2}&#{2+1}", :escape_html => true))
    assert_equal("<p>4&amp;3</p>\n", render("%p&== #{2+2}&#{2+1}", :escape_html => false))
  end

  def test_unescaped_inline_string_interpolation
    assert_equal("<p>4&3</p>\n", render("%p!== #{2+2}&#{2+1}", :escape_html => true))
    assert_equal("<p>4&3</p>\n", render("%p!== #{2+2}&#{2+1}", :escape_html => false))
  end

  def test_escaped_string_interpolation
    assert_equal("<p>\n  4&amp;3\n</p>\n", render("%p\n  &== #{2+2}&#{2+1}", :escape_html => true))
    assert_equal("<p>\n  4&amp;3\n</p>\n", render("%p\n  &== #{2+2}&#{2+1}", :escape_html => false))
  end

  def test_unescaped_string_interpolation
    assert_equal("<p>\n  4&3\n</p>\n", render("%p\n  !== #{2+2}&#{2+1}", :escape_html => true))
    assert_equal("<p>\n  4&3\n</p>\n", render("%p\n  !== #{2+2}&#{2+1}", :escape_html => false))
  end

  def test_scripts_should_respect_escape_html_option
    assert_equal("<p>\n  foo &amp; bar\n</p>\n", render("%p\n  = 'foo & bar'", :escape_html => true))
    assert_equal("<p>\n  foo & bar\n</p>\n", render("%p\n  = 'foo & bar'", :escape_html => false))
  end

  def test_inline_scripts_should_respect_escape_html_option
    assert_equal("<p>foo &amp; bar</p>\n", render("%p= 'foo & bar'", :escape_html => true))
    assert_equal("<p>foo & bar</p>\n", render("%p= 'foo & bar'", :escape_html => false))
  end

  def test_script_ending_in_comment_should_render_when_html_is_escaped
    assert_equal("foo&amp;bar\n", render("= 'foo&bar' #comment", :escape_html => true))
  end

  # Options tests

  def test_filename_and_line
    begin
      render("\n\n = abc", :filename => 'test', :line => 2)
    rescue Exception => e
      assert_kind_of Haml::SyntaxError, e
      assert_match /test:4/, e.backtrace.first
    end

    begin
      render("\n\n= 123\n\n= nil[]", :filename => 'test', :line => 2)
    rescue Exception => e
      assert_kind_of NoMethodError, e
      assert_match /test:6/, e.backtrace.first
    end
  end

  def test_stop_eval
    assert_equal("", render("= 'Hello'", :suppress_eval => true))
    assert_equal("", render("- puts 'foo'", :suppress_eval => true))
    assert_equal("<div id='foo' yes='no' />\n", render("#foo{:yes => 'no'}/", :suppress_eval => true))
    assert_equal("<div id='foo' />\n", render("#foo{:yes => 'no', :call => a_function() }/", :suppress_eval => true))
    assert_equal("<div />\n", render("%div[1]/", :suppress_eval => true))

    begin
      assert_equal("", render(":ruby\n  puts 'hello'", :suppress_eval => true))
    rescue Haml::Error => err
      caught = true
      assert_equal('Filter "ruby" is not defined.', err.message)
    end
    assert(caught, "Rendering a ruby filter without evaluating didn't throw an error!")
  end

  def test_attr_wrapper
    assert_equal("<p strange=*attrs*>\n</p>\n", render("%p{ :strange => 'attrs'}", :attr_wrapper => '*'))
    assert_equal("<p escaped='quo\"te'>\n</p>\n", render("%p{ :escaped => 'quo\"te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"quo'te\">\n</p>\n", render("%p{ :escaped => 'quo\\'te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"q'uo&quot;te\">\n</p>\n", render("%p{ :escaped => 'q\\'uo\"te'}", :attr_wrapper => '"'))
    assert_equal("<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n", render("!!! XML", :attr_wrapper => '"'))
  end

  def test_attrs_parsed_correctly
    assert_equal("<p boom=>biddly='bar =&gt; baz'>\n</p>\n", render("%p{'boom=>biddly' => 'bar => baz'}"))
    assert_equal("<p foo,bar='baz, qux'>\n</p>\n", render("%p{'foo,bar' => 'baz, qux'}"))
    assert_equal("<p escaped='quo\nte'>\n</p>\n", render("%p{ :escaped => \"quo\\nte\"}"))
    assert_equal("<p escaped='quo4te'>\n</p>\n", render("%p{ :escaped => \"quo\#{2 + 2}te\"}"))
  end
  
  def test_correct_parsing_with_brackets
    assert_equal("<p class='foo'>{tada} foo</p>\n", render("%p{:class => 'foo'} {tada} foo"))
    assert_equal("<p class='foo'>deep {nested { things }}</p>\n", render("%p{:class => 'foo'} deep {nested { things }}"))
    assert_equal("<p class='bar foo'>{a { d</p>\n", render("%p{{:class => 'foo'}, :class => 'bar'} {a { d"))
    assert_equal("<p foo='bar'>a}</p>\n", render("%p{:foo => 'bar'} a}"))
    
    foo = []
    foo[0] = Struct.new('Foo', :id).new
    assert_equal("<p class='struct_foo' id='struct_foo_new'>New User]</p>\n",
                 render("%p[foo[0]] New User]", :locals => {:foo => foo}))
    assert_equal("<p class='prefix_struct_foo' id='prefix_struct_foo_new'>New User]</p>\n",
                 render("%p[foo[0], :prefix] New User]", :locals => {:foo => foo}))

    foo[0].id = 1
    assert_equal("<p class='struct_foo' id='struct_foo_1'>New User]</p>\n",
                 render("%p[foo[0]] New User]", :locals => {:foo => foo}))
    assert_equal("<p class='prefix_struct_foo' id='prefix_struct_foo_1'>New User]</p>\n",
                 render("%p[foo[0], :prefix] New User]", :locals => {:foo => foo}))
  end
  
  def test_empty_attrs
    assert_equal("<p attr=''>empty</p>\n", render("%p{ :attr => '' } empty"))
    assert_equal("<p attr=''>empty</p>\n", render("%p{ :attr => x } empty", :locals => {:x => ''}))
  end
  
  def test_nil_attrs
    assert_equal("<p>nil</p>\n", render("%p{ :attr => nil } nil"))
    assert_equal("<p>nil</p>\n", render("%p{ :attr => x } nil", :locals => {:x => nil}))
  end

  def test_nil_id_with_syntactic_id
    assert_equal("<p id='foo'>nil</p>\n", render("%p#foo{:id => nil} nil"))
    assert_equal("<p id='foo_bar'>nil</p>\n", render("%p#foo{{:id => 'bar'}, :id => nil} nil"))
    assert_equal("<p id='foo_bar'>nil</p>\n", render("%p#foo{{:id => nil}, :id => 'bar'} nil"))
  end

  def test_nil_class_with_syntactic_class
    assert_equal("<p class='foo'>nil</p>\n", render("%p.foo{:class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.bar.foo{:class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.foo{{:class => 'bar'}, :class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.foo{{:class => nil}, :class => 'bar'} nil"))
  end

  def test_locals
    assert_equal("<p>Paragraph!</p>\n", render("%p= text", :locals => { :text => "Paragraph!" }))
  end

  def test_deprecated_locals_option
    Kernel.module_eval do
      def warn_with_stub(msg); end
      alias_method :warn_without_stub, :warn
      alias_method :warn, :warn_with_stub
    end

    assert_equal("<p>Paragraph!</p>\n", Haml::Engine.new("%p= text", :locals => { :text => "Paragraph!" }).render)

    Kernel.module_eval { alias_method :warn, :warn_without_stub }
  end

  def test_dynamic_attrs_shouldnt_register_as_literal_values
    assert_equal("<p a='b2c'>\n</p>\n", render('%p{:a => "b#{1 + 1}c"}'))
    assert_equal("<p a='b2c'>\n</p>\n", render("%p{:a => 'b' + (1 + 1).to_s + 'c'}"))
  end

  def test_dynamic_attrs_with_self_closed_tag
    assert_equal("<a b='2' />\nc\n", render("%a{'b' => 1 + 1}/\n= 'c'\n"))
  end

  def test_exceptions
    EXCEPTION_MAP.each do |key, value|
      begin
        render(key)
      rescue Exception => err
        value = [value] unless value.is_a?(Array)

        assert_equal(value.first, err.message, "Line: #{key}")
        assert_equal(value[1] || key.split("\n").length, err.backtrace[0].gsub('(haml):', '').to_i, "Line: #{key}")
      else
        assert(false, "Exception not raised for\n#{key}")
      end
    end
  end

  def test_exception_line
    render("a\nb\n!!!\n  c\nd")
  rescue Haml::SyntaxError => e
    assert_equal("(haml):4", e.backtrace[0])
  else
    assert(false, '"a\nb\n!!!\n  c\nd" doesn\'t produce an exception')
  end

  def test_exception
    render("%p\n  hi\n  %a= undefined\n= 12")
  rescue Exception => e
    assert_match("(haml):3", e.backtrace[0])
  else
    # Test failed... should have raised an exception
    assert(false)
  end

  def test_compile_error
    render("a\nb\n- fee)\nc")
  rescue Exception => e
    assert_match(/^compile error\n\(haml\):3: syntax error/i, e.message)
  else
    assert(false,
           '"a\nb\n- fee)\nc" doesn\'t produce an exception!')
  end

  def test_unbalanced_brackets
    render('== #{1 + 5} foo #{6 + 7 bar #{8 + 9}')
  rescue Haml::SyntaxError => e
    assert_equal("Unbalanced brackets.", e.message)
  end

  def test_balanced_conditional_comments
    assert_equal("<!--[if !(IE 6)|(IE 7)]> Bracket: ] <![endif]-->\n",
                 render("/[if !(IE 6)|(IE 7)] Bracket: ]"))
  end

  def test_no_bluecloth
    Kernel.module_eval do
      def gem_original_require_with_bluecloth(file)
        raise LoadError if file == 'bluecloth'
        gem_original_require_without_bluecloth(file)
      end
      alias_method :gem_original_require_without_bluecloth, :gem_original_require
      alias_method :gem_original_require, :gem_original_require_with_bluecloth
    end

    begin
      assert_equal("<h1>Foo</h1>\t<p>- a\n- b</p>\n",
                   Haml::Engine.new(":markdown\n  Foo\n  ===\n  - a\n  - b").to_html)
    rescue Haml::Error => e
      if e.message == "Can't run Markdown filter; required 'bluecloth' or 'redcloth', but none were found"
        puts "\nCouldn't require 'bluecloth' or 'redcloth'; skipping a test."
      else
        raise e
      end
    end

    Kernel.module_eval do
      alias_method :gem_original_require, :gem_original_require_without_bluecloth
    end
  end

  def test_no_redcloth
    Kernel.module_eval do
      def gem_original_require_with_redcloth(file)
        raise LoadError if file == 'redcloth'
        gem_original_require_without_redcloth(file)
      end
      alias_method :gem_original_require_without_redcloth, :gem_original_require
      alias_method :gem_original_require, :gem_original_require_with_redcloth
    end

    begin
      Haml::Engine.new(":redcloth\n  _foo_").to_html
    rescue Haml::Error
    else
      assert(false, "No exception raised!")
    end

    Kernel.module_eval do
      alias_method :gem_original_require, :gem_original_require_without_redcloth
    end
  end

  def test_no_redcloth_or_bluecloth
    Kernel.module_eval do
      def gem_original_require_with_redcloth_and_bluecloth(file)
        raise LoadError if file == 'redcloth' || file == 'bluecloth'
        gem_original_require_without_redcloth_and_bluecloth(file)
      end
      alias_method :gem_original_require_without_redcloth_and_bluecloth, :gem_original_require
      alias_method :gem_original_require, :gem_original_require_with_redcloth_and_bluecloth
    end

    begin
      Haml::Engine.new(":markdown\n  _foo_").to_html
    rescue Haml::Error
    else
      assert(false, "No exception raised!")
    end

    Kernel.module_eval do
      alias_method :gem_original_require, :gem_original_require_without_redcloth_and_bluecloth
    end    
  end

  def test_empty_filter
    assert_equal(<<END, render(':javascript'))
<script type='text/javascript'>
  //<![CDATA[
    
  //]]>
</script>
END
  end

  def test_local_assigns_dont_modify_class
    assert_equal("bar\n", render("= foo", :locals => {:foo => 'bar'}))
    assert_equal(nil, defined?(foo))
  end

  def test_object_ref_with_nil_id
    user = User.new
    assert_equal("<p class='struct_user' id='struct_user_new'>New User</p>\n",
                 render("%p[user] New User", :locals => {:user => user}))
  end

  def test_object_ref_before_attrs
    user = User.new 42
    assert_equal("<p class='struct_user' id='struct_user_42' style='width: 100px;'>New User</p>\n",
                 render("%p[user]{:style => 'width: 100px;'} New User", :locals => {:user => user}))
  end

  def test_non_literal_attributes
    assert_equal("<p a1='foo' a2='bar' a3='baz' />\n",
                 render("%p{a2, a1, :a3 => 'baz'}/",
                        :locals => {:a1 => {:a1 => 'foo'}, :a2 => {:a2 => 'bar'}}))
  end

  def test_render_should_accept_a_binding_as_scope
    string = "This is a string!"
    string.instance_variable_set("@var", "Instance variable")
    b = string.instance_eval do
      var = "Local variable"
      binding
    end

    assert_equal("<p>THIS IS A STRING!</p>\n<p>Instance variable</p>\n<p>Local variable</p>\n",
                 render("%p= upcase\n%p= @var\n%p= var", :scope => b))
  end

  def test_yield_should_work_with_binding
    assert_equal("12\nFOO\n", render("= yield\n= upcase", :scope => "foo".instance_eval{binding}) { 12 })
  end

  def test_yield_should_work_with_def_method
    s = "foo"
    Haml::Engine.new("= yield\n= upcase").def_method(s, :render)
    assert_equal("12\nFOO\n", s.render { 12 })
  end

  def test_def_method_with_module
    Haml::Engine.new("= yield\n= upcase").def_method(String, :render_haml)
    assert_equal("12\nFOO\n", "foo".render_haml { 12 })
  end

  def test_def_method_locals
    obj = Object.new
    Haml::Engine.new("%p= foo\n.bar{:baz => baz}= boom").def_method(obj, :render, :foo, :baz, :boom)
    assert_equal("<p>1</p>\n<div baz='2' class='bar'>3</div>\n", obj.render(:foo => 1, :baz => 2, :boom => 3))
  end

  def test_render_proc_locals
    proc = Haml::Engine.new("%p= foo\n.bar{:baz => baz}= boom").render_proc(Object.new, :foo, :baz, :boom)
    assert_equal("<p>1</p>\n<div baz='2' class='bar'>3</div>\n", proc[:foo => 1, :baz => 2, :boom => 3])
  end

  def test_render_proc_with_binding
    assert_equal("FOO\n", Haml::Engine.new("= upcase").render_proc("foo".instance_eval{binding}).call)
  end

  def test_ugly_true
    assert_equal("<div id='outer'>\n<div id='inner'>\n<p>hello world</p>\n</div>\n</div>\n",
                 render("#outer\n  #inner\n    %p hello world", :ugly => true))

    assert_equal("<p>#{'s' * 75}</p>\n",
                 render("%p #{'s' * 75}", :ugly => true))

    assert_equal("<p>#{'s' * 75}</p>\n",
                 render("%p= 's' * 75", :ugly => true))
  end

  def test_xhtml_output_option
    assert_equal "<p>\n  <br />\n</p>\n", render("%p\n  %br", :format => :xhtml)
    assert_equal "<a />\n", render("%a/", :format => :xhtml)
  end

  def test_arbitrary_output_option
    assert_raise(Haml::Error, "Invalid output format :html1") { Haml::Engine.new("%br", :format => :html1) }
  end

  # HTML 4.0

  def test_html_has_no_self_closing_tags
    assert_equal "<p>\n  <br>\n</p>\n", render("%p\n  %br", :format => :html4)
    assert_equal "<br>\n", render("%br/", :format => :html4)
  end

  def test_html_renders_empty_node_with_closing_tag
    assert_equal %{<div class='foo'>\n</div>\n}, render(".foo", :format => :html4)
  end

  def test_html_ignores_explicit_self_closing_declaration
    assert_equal "<a>\n</a>\n", render("%a/", :format => :html4)
  end

  def test_html_ignores_xml_prolog_declaration
    assert_equal "", render('!!! XML', :format => :html4)
  end

  def test_html_has_different_doctype
    assert_equal %{<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">\n},
    render('!!!', :format => :html4)
  end

  # because anything before the doctype triggers quirks mode in IE
  def test_xml_prolog_and_doctype_dont_result_in_a_leading_whitespace_in_html
    assert_no_match /^\s+/, render("!!! xml\n!!!", :format => :html4)
  end

  # HTML5
  def test_html5_doctype
    assert_equal %{<!DOCTYPE html>\n}, render('!!!', :format => :html5)
  end
end
