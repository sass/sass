module Haml
  # The module containing the default Haml filters,
  # as well as the base module, {Haml::Filters::Base}.
  #
  # @see Haml::Filters::Base
  module Filters
    # @return [Hash<String, Haml::Filters::Base>] a hash of filter names to classes
    def self.defined
      @defined ||= {}
    end

    # The base module for Haml filters.
    # User-defined filters should be modules including this module.
    # The name of the filter is taken by downcasing the module name.
    # For instance, if the module is named `FooBar`, the filter will be `:foobar`.
    #
    # A user-defined filter should override either \{#render} or {\#compile}.
    # \{#render} is the most common.
    # It takes a string, the filter source,
    # and returns another string, the result of the filter.
    # For example, the following will define a filter named `:sass`:
    #
    #     module Haml::Filters::Sass
    #       include Haml::Filters::Base
    #
    #       def render(text)
    #         ::Sass::Engine.new(text).render
    #       end
    #     end
    #
    # For details on overriding \{#compile}, see its documentation.
    #
    # Note that filters overriding \{#render} automatically support `#{}`
    # for interpolating Ruby code.
    # Those overriding \{#compile} will need to add such support manually
    # if it's desired.
    module Base
      # This method is automatically called when {Base} is included in a module.
      # It automatically defines a filter
      # with the downcased name of that module.
      # For example, if the module is named `FooBar`, the filter will be `:foobar`.
      #
      # @param base [Module, Class] The module that this is included in
      def self.included(base)
        Filters.defined[base.name.split("::").last.downcase] = base
        base.extend(base)
      end

      # Takes the source text that should be passed to the filter
      # and returns the result of running the filter on that string.
      #
      # This should be overridden in most individual filter modules
      # to render text with the given filter.
      # If \{#compile} is overridden, however, \{#render} doesn't need to be.
      #
      # @param text [String] The source text for the filter to process
      # @return [String] The filtered result
      # @raise [Haml::Error] if it's not overridden
      def render(text)
        raise Error.new("#{self.inspect}#render not defined!")
      end

      # Same as \{#render}, but takes a {Haml::Engine} options hash as well.
      # It's only safe to rely on options made available in {Haml::Engine#options\_for\_buffer}.
      #
      # @see #render
      # @param text [String] The source text for the filter to process
      # @return [String] The filtered result
      # @raise [Haml::Error] if it or \{#render} isn't overridden
      def render_with_options(text, options)
        render(text)
      end

      # Same as \{#compile}, but requires the necessary files first.
      # *This is used by {Haml::Engine} and is not intended to be overridden or used elsewhere.*
      #
      # @see #compile
      def internal_compile(*args)
        resolve_lazy_requires
        compile(*args)
      end

      # This should be overridden when a filter needs to have access to the Haml evaluation context.
      # Rather than applying a filter to a string at compile-time,
      # \{#compile} uses the {Haml::Precompiler} instance to compile the string to Ruby code
      # that will be executed in the context of the active Haml template.
      #
      # Warning: the {Haml::Precompiler} interface is neither well-documented
      # nor guaranteed to be stable.
      # If you want to make use of it, you'll probably need to look at the source code
      # and should test your filter when upgrading to new Haml versions.
      #
      # @param precompiler [Haml::Precompiler] The precompiler instance
      # @param text [String] The text of the filter
      # @raise [Haml::Error] if none of \{#compile}, \{#render}, and \{#render_with_options} are overridden
      def compile(precompiler, text)
        resolve_lazy_requires
        filter = self
        precompiler.instance_eval do
          if contains_interpolation?(text)
            return if options[:suppress_eval]

            push_script <<RUBY
find_and_preserve(#{filter.inspect}.render_with_options(#{unescape_interpolation(text)}, _hamlout.options))
RUBY
            return
          end

          rendered = Haml::Helpers::find_and_preserve(filter.render_with_options(text, precompiler.options), precompiler.options[:preserve])

          if !options[:ugly]
            push_text(rendered.rstrip.gsub("\n", "\n#{'  ' * @output_tabs}"))
          else
            push_text(rendered.rstrip)
          end
        end
      end

      # This becomes a class method of modules that include {Base}.
      # It allows the module to specify one or more Ruby files
      # that Haml should try to require when compiling the filter.
      #
      # The first file specified is tried first, then the second, etc.
      # If none are found, the compilation throws an exception.
      #
      # For example:
      #
      #     module Haml::Filters::Markdown
      #       lazy_require 'rdiscount', 'peg_markdown', 'maruku', 'bluecloth'
      #
      #       ...
      #     end
      #
      # @param reqs [Array<String>] The requires to run
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
          classname = self.name.match(/\w+$/)[0]

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

begin
  require 'rubygems'
rescue LoadError; end

module Haml
  module Filters
    # Does not parse the filtered text.
    # This is useful for large blocks of text without HTML tags,
    # when you don't want lines starting with `.` or `-`
    # to be parsed.
    module Plain
      include Base

      # @see Base#render
      def render(text); text; end
    end

    # Surrounds the filtered text with `<script>` and CDATA tags.
    # Useful for including inline Javascript.
    module Javascript
      include Base

      # @see Base#render_with_options
      def render_with_options(text, options)
        <<END
<script type=#{options[:attr_wrapper]}text/javascript#{options[:attr_wrapper]}>
  //<![CDATA[
    #{text.rstrip.gsub("\n", "\n    ")}
  //]]>
</script>
END
      end
    end

    # Surrounds the filtered text with CDATA tags.
    module Cdata
      include Base

      # @see Base#render
      def render(text)
        "<![CDATA[#{("\n" + text).rstrip.gsub("\n", "\n    ")}\n]]>"
      end
    end

    # Works the same as {Plain}, but HTML-escapes the text
    # before placing it in the document.
    module Escaped
      include Base

      # @see Base#render
      def render(text)
        Haml::Helpers.html_escape text
      end
    end

    # Parses the filtered text with the normal Ruby interpreter.
    # All output sent to `$stdout`, such as with `puts`,
    # is output into the Haml document.
    # Not available if the {file:HAML_REFERENCE.md#suppress_eval-option `:suppress_eval`} option is set to true.
    # The Ruby code is evaluated in the same context as the Haml template.
    module Ruby
      include Base
      lazy_require 'stringio'

      # @see Base#compile
      def compile(precompiler, text)
        return if precompiler.options[:suppress_eval]
        precompiler.instance_eval do
          push_silent <<-FIRST.gsub("\n", ';') + text + <<-LAST.gsub("\n", ';')
            _haml_old_stdout = $stdout
            $stdout = StringIO.new(_hamlout.buffer, 'a')
          FIRST
            _haml_old_stdout, $stdout = $stdout, _haml_old_stdout
            _haml_old_stdout.close
          LAST
        end
      end
    end

    # Inserts the filtered text into the template with whitespace preserved.
    # `preserve`d blocks of text aren't indented,
    # and newlines are replaced with the HTML escape code for newlines,
    # to preserve nice-looking output.
    #
    # @see Haml::Helpers#preserve
    module Preserve
      include Base

      # @see Base#render
      def render(text)
        Haml::Helpers.preserve text
      end
    end

    # Parses the filtered text with {Sass} to produce CSS output.
    module Sass
      include Base
      lazy_require 'sass/plugin'

      # @see Base#render
      def render(text)
        ::Sass::Engine.new(text, ::Sass::Plugin.engine_options).render
      end
    end

    # Parses the filtered text with ERB, like an RHTML template.
    # Not available if the {file:HAML_REFERENCE.md#suppress_eval-option `:suppress_eval`} option is set to true.
    # Embedded Ruby code is evaluated in the same context as the Haml template.
    module ERB
      include Base
      lazy_require 'erb'

      # @see Base#compile
      def compile(precompiler, text)
        return if precompiler.options[:suppress_eval]
        src = ::ERB.new(text).src.sub(/^#coding:.*?\n/, '').
          sub(/^_erbout = '';/, "").gsub("\n", ';')
        precompiler.send(:push_silent, src)
      end
    end

    # Parses the filtered text with [Textile](http://www.textism.com/tools/textile).
    # Only works if [RedCloth](http://redcloth.org) is installed.
    module Textile
      include Base
      lazy_require 'redcloth'

      # @see Base#render
      def render(text)
        ::RedCloth.new(text).to_html(:textile)
      end
    end
    RedCloth = Textile
    Filters.defined['redcloth'] = RedCloth

    # Parses the filtered text with [Markdown](http://daringfireball.net/projects/markdown).
    # Only works if [RDiscount](http://github.com/rtomayko/rdiscount),
    # [RPeg-Markdown](http://github.com/rtomayko/rpeg-markdown),
    # [Maruku](http://maruku.rubyforge.org),
    # or [BlueCloth](www.deveiate.org/projects/BlueCloth) are installed.
    module Markdown
      include Base
      lazy_require 'rdiscount', 'peg_markdown', 'maruku', 'bluecloth'

      # @see Base#render
      def render(text)
        engine = case @required
                 when 'rdiscount'
                   ::RDiscount
                 when 'peg_markdown'
                   ::PEGMarkdown
                 when 'maruku'
                   ::Maruku
                 when 'bluecloth'
                   ::BlueCloth
                 end
        engine.new(text).to_html
      end
    end

    # Parses the filtered text with [Maruku](http://maruku.rubyforge.org),
    # which has some non-standard extensions to Markdown.
    module Maruku
      include Base
      lazy_require 'maruku'

      # @see Base#render
      def render(text)
        ::Maruku.new(text).to_html
      end
    end
  end
end
