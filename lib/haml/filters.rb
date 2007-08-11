# This file contains redefinitions of and wrappers around various text
# filters so they can be used as Haml filters.

# :stopdoc:

require 'erb'
require 'sass/engine'
require 'stringio'

begin
  require 'rubygems'
rescue LoadError; end

class ERB; alias_method :render, :result; end

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

    class LazyLoaded
      def initialize(*reqs)
        reqs[0...-1].each do |req|
          begin
            @required = req
            require @required
            return
          rescue LoadError; end # RCov doesn't see this, but it is run
        end
       
        begin
          @required = reqs[-1]
          require @required
        rescue LoadError => e
          classname = self.class.to_s.gsub(/\w+::/, '')

          if reqs.size == 1
            raise HamlError.new("Can't run #{classname} filter; required file '#{reqs.first}' not found")
          else
            raise HamlError.new("Can't run #{classname} filter; required #{reqs.map { |r| "'#{r}'" }.join(' or ')}, but none were found")
          end
        end
      end
    end
    
    class RedCloth < LazyLoaded
      def initialize(text)
        super('redcloth')
        @engine = ::RedCloth.new(text)
      end

      def render
        @engine.to_html
      end
    end
      
    # Uses RedCloth to provide only Textile (not Markdown) parsing
    class Textile < RedCloth
      def render
        @engine.to_html(:textile)
      end
    end

    # Uses BlueCloth or RedCloth to provide only Markdown (not Textile) parsing
    class Markdown < LazyLoaded
      def initialize(text)
        super('bluecloth', 'redcloth')

        if @required == 'bluecloth'
          @engine = ::BlueCloth.new(text)
        else
          @engine = ::RedCloth.new(text)
        end
      end

      def render
        if @engine.is_a?(::BlueCloth)
          @engine.to_html
        else
          @engine.to_html(:markdown)
        end
      end
    end
  end
end

# :startdoc:
