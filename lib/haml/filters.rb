# This file contains redefinitions of and wrappers around various text
# filters so they can be used as Haml filters.

module Haml
  # The module containing the default filters,
  # as well as the base module,
  # Haml::Filters::Base.
  module Filters
    # The base module for Haml filters.
    # User-defined filters should be modules including this module.
    #
    # A user-defined filter should override either Base#render or Base #compile.
    # Base#render is the most common.
    # It takes a string, the filter source,
    # and returns another string,
    # the result of the filter.
    # For example:
    #
    #   module Haml::Filters::Sass
    #     include Haml::Filters::Base
    #
    #     def render(text)
    #       ::Sass::Engine.new(text).render
    #     end
    #   end
    #
    # For details on overriding #compile, see its documentation.
    #
    module Base
      def self.included(base) # :nodoc:
        base.extend(base)
      end

      # Takes a string, the source text that should be passed to the filter,
      # and returns the string resulting from running the filter on <tt>text</tt>.
      #
      # This should be overridden in most individual filter modules
      # to render text with the given filter.
      # If compile is overridden, however, render doesn't need to be.
      def render(text)
        raise Error.new("#{self.inspect}#render not defined!")
      end

      def internal_compile(*args) # :nodoc:
        resolve_lazy_requires
        compile(*args)
      end

      # compile should be overridden when a filter needs to have access
      # to the Haml evaluation context.
      # Rather than applying a filter to a string at compile-time,
      # compile uses the Haml::Precompiler instance to compile the string to Ruby code
      # that will be executed in the context of the active Haml template.
      #
      # Warning: the Haml::Precompiler interface is neither well-documented
      # nor guaranteed to be stable.
      # If you want to make use of it,
      # you'll probably need to look at the source code
      # and should test your filter when upgrading to new Haml versions.
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

      # This becomes a class method of modules that include Base.
      # It allows the module to specify one or more Ruby files
      # that Haml should try to require when compiling the filter.
      #
      # The first file specified is tried first,
      # then the second, etc.
      # If none are found, the compilation throws an exception.
      #
      # For example:
      #
      #   module Haml::Filters::Markdown
      #     lazy_require 'bluecloth', 'redcloth'
      #
      #     ...
      #   end
      #
      def lazy_require(*reqs)
        @lazy_requires = reqs
      end

      private

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
            raise Error.new("Can't run #{classname} filter; required file '#{@lazy_requires.first}' not found")
          else
            raise Error.new("Can't run #{classname} filter; required #{@lazy_requires.map { |r| "'#{r}'" }.join(' or ')}, but none were found")
          end
        end
      end
    end
  end
end

# :stopdoc:

begin
  require 'rubygems'
rescue LoadError; end

module Haml
  module Filters
    module Plain
      include Base

      def render(text); text; end
    end

    module Javascript
      include Base

      def render(text)
        <<END
<script type='text/javascript'>
  //<![CDATA[
    #{text.rstrip.gsub("\n", "\n    ")}
  //]]>
</script>
END
      end
    end

    module Ruby
      include Base
      lazy_require 'stringio'

      def compile(precompiler, text)
        precompiler.instance_eval do
          push_silent <<-END.gsub("\n", ';')
            _haml_old_stdout = $stdout
            $stdout = StringIO.new(_hamlout.buffer, 'a')
            #{text}
            _haml_old_stdout, $stdout = $stdout, _haml_old_stdout
            _haml_old_stdout.close
          END
        end
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

      def compile(precompiler, text)
        src = ::ERB.new(text).src.sub(/^_erbout = '';/, "").gsub("\n", ';')
        precompiler.send(:push_silent, src)
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
