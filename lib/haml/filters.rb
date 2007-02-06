# This file contains redefinitions of and wrappers around various text
# filters so they can be used as Haml filters.

# :stopdoc:

require 'erb'
require 'sass/engine'
require 'stringio'

volatile_requires = ['rubygems', 'redcloth', 'bluecloth']
NOT_LOADED = [] unless defined?(NOT_LOADED)
volatile_requires.each do |file|
  begin
    require file
  rescue LoadError
    NOT_LOADED.push file
  end
end

class ERB; alias_method :render, :result; end

unless NOT_LOADED.include? 'bluecloth'
  class BlueCloth; alias_method :render, :to_html; end
end

module Haml
  module Filters
    class Plain
      def initialize(text)
        @text = text
      end

      def render
        @text
      end
    end

    class Ruby
      def initialize(text)
        @text = text
      end

      def render
        old_stdout = $stdout
        $stdout = StringIO.new
        Object.new.instance_eval(@text)
        old_stdout, $stdout = $stdout, old_stdout
        old_stdout.pos = 0
        old_stdout.read
      end
    end

    class Preserve
      def initialize(text)
        @text = text
      end

      def render
        Haml::Helpers.preserve(@text)
      end
    end

    unless NOT_LOADED.include? 'bluecloth'
      Markdown = BlueCloth unless defined?(Markdown)
    end

    unless NOT_LOADED.include? 'redcloth'
      class ::RedCloth; alias_method :render, :to_html; end
      
      # Uses RedCloth to provide only Textile (not Markdown) parsing
      class Textile < RedCloth
        def render
          self.to_html(:textile)
        end
      end

      unless defined?(Markdown)
        # Uses RedCloth to provide only Markdown (not Textile) parsing
        class Markdown < RedCloth
          def render
            self.to_html(:markdown)
          end
        end
      end
    end
  end
end

# :startdoc:
