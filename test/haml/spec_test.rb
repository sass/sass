#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

begin
  require 'json'
rescue LoadError
end

class SpecTest < Test::Unit::TestCase
  spec_file = File.dirname(__FILE__) + '/spec/tests.json'
  if !File.exists?(spec_file)
    error = <<MSG.rstrip
Couldn't load haml-spec, skipping some tests.
To use haml-spec, run `git submodule update --init`
MSG
  elsif !defined?(JSON)
    error = "Couldn't load json, skipping some tests."
  end

  if error
    define_method(:test_fake_couldnt_load_spec) {warn("\n" + error)}
  else
    JSON.parse(File.read(spec_file)).each do |name, tests|
      tests.each do |subname, test|
        define_method("test_spec: #{name} (#{subname})") do
          options = convert_hash(test["config"])
          options[:format] = options[:format].to_sym if options[:format]
          engine = Haml::Engine.new(test["haml"], options)

          result = engine.render(Object.new, convert_hash(test["locals"]))

          assert_equal(test["html"], result.rstrip)
        end
      end
    end
  end

  private

  def convert_hash(hash)
    return {} unless hash
    Haml::Util.map_keys(hash) {|k| k.to_sym}
  end
end
