#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

begin
  require 'json'
rescue LoadError
end

class SpecTest < Test::Unit::TestCase
  spec_file = File.dirname(__FILE__) + '/spec/tests.json'
  if !File.exists?(spec_file)
    warn <<MSG
Couldn't load haml-spec, skipping some tests.
To use haml-spec, run `git submodule update --init`
MSG
  elsif !defined?(JSON)
    warn "Couldn't load json, skipping some tests."
  else
    JSON.parse(File.read(spec_file)).each do |name, tests|
      define_method("test_spec: #{name}") do
        tests.each do |haml, html|
          result = Haml::Engine.new(haml, :locals => [:var]).render(
            Object.new,
            :var => "value",
            :first => "a",
            :last => "z")
          assert_equal(html, result.rstrip)
        end
      end
    end
  end
end
