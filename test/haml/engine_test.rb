#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/haml'
require 'haml/engine'

class EngineTest < Test::Unit::TestCase

  def render(text, options = {})
    Haml::Engine.new(text, options).to_html
  end

  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  # This is ugly because Hashes are unordered; we don't always know the order
  # in which attributes will be returned.
  # There is probably a better way to do this.
  def test_attributes_should_render_correctly
    assert_equal("<div class='atlantis' style='ugly'>\n</div>", render(".atlantis{:style => 'ugly'}").chomp)
  rescue
    assert_equal("<div style='ugly' class='atlantis'>\n</div>", render(".atlantis{:style => 'ugly'}").chomp)
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

  def test_nil_tag_value_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= nil"))
  end

  def test_tag_with_failed_if_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= 'Hello' if false"))
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
      assert_equal('Filter "ruby" is not defined!', err.message)
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

  def test_locals
    assert_equal("<p>Paragraph!</p>\n", render("%p= text", :locals => { :text => "Paragraph!" }))
  end
  
  def test_recompile_with_new_locals
    template = "%p= (text == 'first time') ? text : new_text"
    assert_equal("<p>first time</p>\n", render(template, :locals => { :text => "first time" }))
    assert_equal("<p>second time</p>\n", render(template, :locals => { :text => "recompile", :new_text => "second time" }))

    # Make sure the method called will return junk unless recompiled
    method_name = Haml::Engine.send(:class_variable_get, '@@method_names')[template]
    Haml::Engine::CompiledTemplates.module_eval "def #{method_name}(stuff); @haml_stack[-1].push_text 'NOT RECOMPILED', 0; end"

    assert_equal("NOT RECOMPILED\n", render(template, :locals => { :text => "first time" }))
    assert_equal("<p>first time</p>\n", render(template, :locals => { :text => "first time", :foo => 'bar' }))
  end

  def test_dynamc_attrs_shouldnt_register_as_literal_values
    assert_equal("<p a='b2c'>\n</p>\n", render('%p{:a => "b#{1 + 1}c"}'))
    assert_equal("<p a='b2c'>\n</p>\n", render("%p{:a => 'b' + (1 + 1).to_s + 'c'}"))
  end

  def test_rec_merge
    hash1 = {1=>2, 3=>{5=>7, 8=>9}}
    hash2 = {4=>5, 3=>{5=>2, 16=>12}}
    hash3 = {1=>2, 4=>5, 3=>{5=>2, 8=>9, 16=>12}}

    hash1.rec_merge!(hash2)
    assert_equal(hash3, hash1)
  end

  def test_exception_type
    begin
      render("%p hi\n= undefined")
    rescue Exception => e
      assert(e.is_a?(Haml::Error))
      assert_equal(2, e.haml_line)
      assert_equal(nil, e.haml_filename)
      assert_equal('(haml):2', e.backtrace[0])
    else
      # Test failed... should have raised an exception
      assert(false)
    end
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
        assert(e.is_a?(Haml::Error),
               "#{err.dump} doesn't produce Haml::SyntaxError!")
      else
        assert(false,
               "#{err.dump} doesn't produce an exception!")
      end
    end
  end

  def test_compile_error
    begin
      render("a\nb\n- fee do\nc")
    rescue Exception => e
      assert_equal(3, e.haml_line)
    else
      assert(false,
             '"a\nb\n- fee do\nc" doesn\'t produce an exception!')
    end
  end

  def test_no_bluecloth
    old_markdown = false
    if defined?(Haml::Filters::Markdown)
      old_markdown = Haml::Filters::Markdown
    end

    Kernel.module_eval do
      alias_method :haml_old_require, :gem_original_require

      def gem_original_require(file)
        raise LoadError if file == 'bluecloth'
        haml_old_require(file)
      end
    end
    
    if old_markdown
      Haml::Filters.instance_eval do
        remove_const 'Markdown'
      end
    end

    # This is purposefully redundant, so it doesn't stop
    # haml/filters from being required later on.
    require 'haml/../haml/filters'

    assert_equal("<h1>Foo</h1>\t<p>- a\n- b</p>\n",
                 Haml::Engine.new(":markdown\n  Foo\n  ===\n  - a\n  - b").to_html)

    Haml::Filters.instance_eval do
      remove_const 'Markdown'
    end

    Haml::Filters.const_set('Markdown', old_markdown) if old_markdown

    Kernel.module_eval do
      alias_method :gem_original_require, :haml_old_require
    end

    NOT_LOADED.delete 'bluecloth'
  end

  def test_no_redcloth
    Kernel.module_eval do
      alias_method :haml_old_require2, :gem_original_require

      def gem_original_require(file)
        raise LoadError if file == 'redcloth'
        haml_old_require2(file)
      end
    end

    # This is purposefully redundant, so it doesn't stop
    # haml/filters from being required later on.
    require 'haml/../haml/../haml/filters'

    begin
      Haml::Engine.new(":redcloth\n  _foo_").to_html
    rescue Haml::HamlError
    else
      assert(false, "No exception raised!")
    end

    Kernel.module_eval do
      alias_method :gem_original_require2, :haml_old_require
    end

    NOT_LOADED.delete 'redcloth'
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
end
