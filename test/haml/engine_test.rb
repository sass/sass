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

  # Options tests

  def test_stop_eval
    assert_equal("", render("= 'Hello'", :suppress_eval => true))
  end

  def test_attr_wrapper
    assert_equal("<p strange=*attrs*>\n</p>\n", render("%p{ :strange => 'attrs'}", :attr_wrapper => '*'))
    assert_equal("<p escaped='quo\"te'>\n</p>\n", render("%p{ :escaped => 'quo\"te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"q'uo&quot;te\">\n</p>\n", render("%p{ :escaped => 'q\\'uo\"te'}", :attr_wrapper => '"'))
    assert_equal("<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n", render("!!! XML", :attr_wrapper => '"'))
  end

  def test_locals
    assert_equal("<p>Paragraph!</p>\n", render("%p= text", :locals => { :text => "Paragraph!" }))
  end

  def test_precompiled
    precompiled = <<-END
      def _haml_render
        _hamlout = @haml_stack[-1]
        _erbout = _hamlout.buffer

        _hamlout.open_tag("p", 0, nil, true, "", nil, nil, false)
        @haml_lineno = 1
        haml_temp =  "Haml Rocks Socks"
        haml_temp = _hamlout.push_script(haml_temp, 1, false)
        _hamlout.close_tag("p", 0)
      end
    END

    assert_equal("<p>Haml Rocks Socks</p>\n", render("%h1 I shall not be rendered", :precompiled => precompiled))
  end
  
  def test_comps
    assert_equal(-1, "foo" <=> nil)
    assert_equal(1, nil <=> "foo")
  end

  def test_rec_merge
    hash1 = {1=>2, 3=>{5=>7, 8=>9}}
    hash1_2 = hash1.clone
    hash2 = {4=>5, 3=>{5=>2, 16=>12}}
    hash3 = {1=>2, 4=>5, 3=>{5=>2, 8=>9, 16=>12}}

    assert_equal(hash3, hash1.rec_merge(hash2))
    assert_equal(hash1_2, hash1)
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
      "a\n%p~\nb", "a\n~\nb", "%p/\n  a", "%p\n \t%a b",
      "%a\n b\nc", "%a\n    b\nc",
      ":notafilter\n  This isn't\n  a filter!",
    ]
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
end
