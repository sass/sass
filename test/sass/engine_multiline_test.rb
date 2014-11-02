#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/engine'
require 'stringio'
require 'mock_importer'
require 'pathname'

class SassEngineMultilineTest < MiniTest::Test

  def test_single_line_map
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: (a: 1, b: 2); }
CSS
$foo: (a: 1, b: 2)

.debug
  inspect: inspect($foo)
SASS
  end

  def test_indented_single_line_map
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: (a: 1, b: 2); }
CSS
$foo:
  (a: 1, b: 2)

.debug
  inspect: inspect($foo)
SASS
  end

  def test_multiline_map
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: (a: 1, b: 2); }
CSS
$foo:
  a: 1,
  b: 2

.debug
  inspect: inspect($foo)
SASS
  end

  def test_single_line_list
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: 1, 2; }
CSS
$foo: 1, 2

.debug
  inspect: inspect($foo)
SASS
  end

  def test_indented_single_line_list
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: 1, 2; }
CSS
$foo:
  1, 2

.debug
  inspect: inspect($foo)
SASS
  end

  def test_multiline_list
    assert_equal(<<CSS, render(<<SASS))
.debug {
  inspect: "one", 2, "three"; }
CSS
$foo:
  "one",
  2,
  "three"

.debug
  inspect: inspect($foo)
SASS
  end

  def test_multiline_sassscript_errors
    assert_raise_message(Sass::SyntaxError,
      "Invalid CSS after \"foo-bar )\": expected expression (e.g. 1px, bold), was \")\"") do
      render(<<SASS)
$foo:
  valid-syntax
  foo-bar )

.debug
  inspect: inspect($foo)
SASS
    end
  end

  private

  def render(sass, options = {})
    munge_filename options
    options[:importer] ||= MockImporter.new
    Sass::Engine.new(sass, options).render
  end

end
