#!/usr/bin/env ruby

require 'rubygems'
require 'active_support'
require 'action_controller'
require 'action_view'

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/haml'
require 'haml/engine'

class EngineTest < Test::Unit::TestCase

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

  def test_long_liner_should_not_print_on_one_line
    assert_equal("<div>\n  #{'x' * 51}\n</div>", render("%div #{'x' * 51}").chomp)
  end
  
  def test_non_prerendered_one_liner
    assert_equal("<p class='awesome'>One line</p>\n", render("%p{:class => c} One line", :locals => {:c => 'awesome'}))
  end
  
  def test_non_prerendered_script_one_liner
    assert_equal("<p class='awesome'>One line</p>\n", render("%p{:class => c}= 'One line'", :locals => {:c => 'awesome'}))
  end
  
  def test_non_prerendered_long_script_one_liner
    assert_equal("<p class='awesome'>\n  #{'x' * 60}\n</p>\n", render("%p{:class => c}= 'x' * 60", :locals => {:c => 'awesome'}))
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

  # Options tests

  def test_stop_eval
    assert_equal("", render("= 'Hello'", :suppress_eval => true))
    assert_equal("", render("- puts 'foo'", :suppress_eval => true))
    assert_equal("<div id='foo' yes='no' />\n", render("#foo{:yes => 'no'}/", :suppress_eval => true))
    assert_equal("<div id='foo' />\n", render("#foo{:yes => 'no', :call => a_function() }/", :suppress_eval => true))
    assert_equal("<div />\n", render("%div[1]/", :suppress_eval => true))

    begin
      assert_equal("", render(":ruby\n  puts 'hello'", :suppress_eval => true))
    rescue Haml::HamlError => err
      caught = true
      assert_equal('"ruby" filter is not defined!', err.message)
    end
    assert(caught, "Rendering a ruby filter without evaluating didn't throw an error!")
  end

  def test_attr_wrapper
    assert_equal("<p strange=*attrs*>\n</p>\n", render("%p{ :strange => 'attrs'}", :attr_wrapper => '*'))
    assert_equal("<p escaped='quo\"te'>\n</p>\n", render("%p{ :escaped => 'quo\"te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"q'uo&quot;te\">\n</p>\n", render("%p{ :escaped => 'q\\'uo\"te'}", :attr_wrapper => '"'))
    assert_equal("<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n", render("!!! XML", :attr_wrapper => '"'))
  end

  def test_attrs_parsed_correctly
    assert_equal("<p boom=>biddly='bar => baz'>\n</p>\n", render("%p{'boom=>biddly' => 'bar => baz'}"))
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

  def test_rec_merge
    hash1 = {1=>2, 3=>{5=>7, 8=>9}}
    hash2 = {4=>5, 3=>{5=>2, 16=>12}}
    hash3 = {1=>2, 4=>5, 3=>{5=>2, 8=>9, 16=>12}}

    hash1.rec_merge!(hash2)
    assert_equal(hash3, hash1)
  end

  def test_syntax_errors
    errs = [ "!!!\n  a", "a\n  b", "a\n:foo\nb", "/ a\n  b",
             "% a", "%p a\n  b", "a\n%p=\nb", "%p=\n  a",
             "a\n%p~\nb", "a\n~\nb", "a\n~\n  b", "%p~\n  b", "%p/\n  a",
             "%p\n \t%a b", "%a\n b\nc", "%a\n    b\nc",
             ":notafilter\n  This isn't\n  a filter!",
             ".{} a", "\#{} a", ".= 'foo'", "%a/ b", "%p..class", "%p..#." ]
    errs.each do |err|
      begin
        render(err)
      rescue Exception => e
        assert(e.is_a?(Haml::Error), "#{err.dump} doesn't produce Haml::SyntaxError")
      else
        assert(false, "#{err.dump} doesn't produce an exception")
      end
    end
  end

  def test_syntax_error
    render("a\nb\n!!!\n  c\nd")
  rescue Haml::SyntaxError => e
    assert_equal(e.message, "Illegal Nesting: Nesting within a header command is illegal.")
    assert_equal("(haml):3", e.backtrace[0])
  rescue Exception => e
    assert(false, '"a\nb\n!!!\n  c\nd" doesn\'t produce a Haml::SyntaxError')
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
    rescue Haml::HamlError => e
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
    rescue Haml::HamlError
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
    rescue Haml::HamlError
    else
      assert(false, "No exception raised!")
    end

    Kernel.module_eval do
      alias_method :gem_original_require, :gem_original_require_without_redcloth_and_bluecloth
    end    
  end

  def test_local_assigns_dont_modify_class
    assert_equal("bar\n", render("= foo", :locals => {:foo => 'bar'}))
    assert_equal(nil, defined?(foo))
  end

  def test_object_ref_with_nil_id
    user = Struct.new('User', :id).new
    assert_equal("<p class='struct_user' id='struct_user_new'>New User</p>\n",
                 render("%p[user] New User", :locals => {:user => user}))
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
end
