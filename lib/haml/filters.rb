# This file contains redefinitions of and wrappers around various text
# filters so they can be used as Haml filters.

# :stopdoc:

begin
  require 'rubygems'
rescue LoadError; end

module Haml
  module Filters
    module Base
      def self.included(base)
        base.extend(base)
      end

      def compile(precompiler, text)
        resolve_lazy_requires
        filter = self
        precompiler.instance_eval do
          if contains_interpolation?(text)
            return if options[:suppress_eval]

            push_script("#{filter.inspect}.render(#{unescape_interpolation(text)})", false)
            return
          end

          rendered = filter.render(text)

          if !options[:ugly]
            push_text(rendered.rstrip.gsub("\n", "\n#{'  ' * @output_tabs}"))
          else
            push_text(rendered.rstrip)
          end
        end
      end

      def lazy_require(*reqs)
        @lazy_requires = reqs
      end

      def resolve_lazy_requires
        return unless @lazy_requires

        @lazy_requires[0...-1].each do |req|
          begin
            @required = req
            require @required
            return
          rescue LoadError; end # RCov doesn't see this, but it is run
        end
       
        begin
          @required = @lazy_requires[-1]
          require @required
        rescue LoadError => e
          classname = self.class.to_s.gsub(/\w+::/, '')

          if @lazy_requires.size == 1
            raise HamlError.new("Can't run #{classname} filter; required file '#{@lazy_requires.first}' not found")
          else
            raise HamlError.new("Can't run #{classname} filter; required #{@lazy_requires.map { |r| "'#{r}'" }.join(' or ')}, but none were found")
          end
        end
      end
    end

    module Plain
      include Base

      def render(text); text; end
    end

    module Ruby
      include Base
      lazy_require 'stringio'

      def render(text)
        old_stdout = $stdout
        $stdout = StringIO.new
        Object.new.instance_eval(text)
        old_stdout, $stdout = $stdout, old_stdout
        old_stdout.pos = 0
        old_stdout.read
      end
    end

    module Preserve
      include Base

      def compile(precompiler, text)
        text = Haml::Helpers.preserve(text) + "\n"

        precompiler.instance_eval do
          if contains_interpolation?(text)
            return if options[:suppress_eval]

            push_silent("_hamlout.buffer << #{unescape_interpolation(text)};")
            return
          end

          concat_merged_text(text)
        end
      end
    end

    module Sass
      include Base
      lazy_require 'sass/engine'

      def render(text)
        ::Sass::Engine.new(text).render
      end
    end

    module ERB
      include Base
      lazy_require 'erb'

      def render(text)
        ::ERB.new(text).result(binding)
      end
    end
    
    module RedCloth
      include Base
      lazy_require 'redcloth'

      def render(text)
        ::RedCloth.new(text).to_html
      end
    end
      
    # Uses RedCloth to provide only Textile (not Markdown) parsing
    module Textile
      include Base
      lazy_require 'redcloth'

      def render(text)
        ::RedCloth.new(text).to_html(:textile)
      end
    end

    # Uses BlueCloth or RedCloth to provide only Markdown (not Textile) parsing
    module Markdown
      include Base
      lazy_require 'bluecloth', 'redcloth'

      def render(text)
        if @required == 'bluecloth'
          ::BlueCloth.new(text).to_html
        else
          ::RedCloth.new(text).to_html(:markdown)
        end
      end
    end
  end
end

# :startdoc:
