require 'cgi'
require 'erubis'
require 'ruby_parser'

module Haml
  class HTML
    # A class for converting ERB code into a format that's easier
    # for the {Haml::HTML} Hpricot-based parser to understand.
    #
    # Uses [Erubis](http://www.kuwata-lab.com/erubis)'s extensible parsing powers
    # to parse the ERB in a reliable way,
    # and [ruby_parser](http://parsetree.rubyforge.org/)'s Ruby knowledge
    # to figure out whether a given chunk of Ruby code starts a block or not.
    #
    # The ERB tags are converted to HTML tags in the following way.
    # `<% ... %>` is converted into `<haml:silent> ... </haml:silent>`.
    # `<%= ... %>` is converted into `<haml:loud> ... </haml:loud>`.
    # Finally, if either of these opens a Ruby block,
    # `<haml:block> ... </haml:block>` will wrap the entire contents of the block -
    # that is, everything that should be indented beneath the previous silent or loud tag.
    class ERB < Erubis::Basic::Engine
      # Compiles an ERB template into a HTML document containing `haml:` tags.
      #
      # @param template [String] The ERB template
      # @return [String] The output document
      # @see Haml::HTML::ERB
      def self.compile(template)
        new(template).src
      end

      # `html2haml` doesn't support HTML-escaped expressions.
      def escaped_expr(code)
        raise Haml::Error.new("html2haml doesn't support escaped expressions.")
      end

      # The ERB-to-Hamlized-HTML conversion has no preamble.
      def add_preamble(src); end

      # The ERB-to-Hamlized-HTML conversion has no postamble.
      def add_postamble(src); end

      # Concatenates the text onto the source buffer.
      #
      # @param src [String] The source buffer
      # @param text [String] The raw text to add to the buffer
      def add_text(src, text)
        src << text
      end

      # Concatenates a silent Ruby statement onto the source buffer.
      # This uses the `<haml:silent>` tag,
      # and may close and/or open a Ruby block with the `<haml:block>` tag.
      #
      # In particular, a block is closed if this statement is some form of `end`,
      # opened if it's a block opener like `do`, `if`, or `begin`,
      # and both closed and opened if it's a mid-block keyword
      # like `else` or `when`.
      #
      # @param src [String] The source buffer
      # @param code [String] The Ruby statement to add to the buffer
      def add_stmt(src, code)
        src << '</haml:block>' if block_closer?(code) || mid_block?(code)
        src << '<haml:silent>' << h(code) << '</haml:silent>' unless code.strip == "end"
        src << '<haml:block>' if block_opener?(code) || mid_block?(code)
      end

      # Concatenates a Ruby expression that's printed to the document
      # onto the source buffer.
      # This uses the `<haml:silent>` tag,
      # and may open a Ruby block with the `<haml:block>` tag.
      # An expression never closes a block.
      #
      # @param src [String] The source buffer
      # @param code [String] The Ruby expression to add to the buffer
      def add_expr_literal(src, code)
        src << '<haml:loud>' << h(code) << '</haml:loud>'
        src << '<haml:block>' if block_opener?(code)
      end

      # `html2haml` doesn't support debugging expressions.
      def add_expr_debug(src, code)
        raise Haml::Error.new("html2haml doesn't support debugging expressions.")
      end

      private

      # HTML-escaped some text (in practice, always Ruby code).
      # A utility method.
      #
      # @param text [String] The text to escape
      # @return [String] The escaped text
      def h(text)
        CGI.escapeHTML(text)
      end

      # Returns whether the code is valid Ruby code on its own.
      #
      # @param code [String] Ruby code to check
      # @return [Boolean]
      def valid_ruby?(code)
        RubyParser.new.parse(code)
      rescue Racc::ParseError => e
        false
      end

      # Checks if a string of Ruby code opens a block.
      # This could either be something like `foo do |a|`
      # or a keyword that requires a matching `end`
      # like `if`, `begin`, or `case`.
      #
      # @param code [String] Ruby code to check
      # @return [Boolean]
      def block_opener?(code)
        valid_ruby?(code + "\nend") ||
          valid_ruby?(code + "\nwhen foo\nend")
      end

      # Checks if a string of Ruby code closes a block.
      # This is always `end` followed optionally by some method calls.
      #
      # @param code [String] Ruby code to check
      # @return [Boolean]
      def block_closer?(code)
        valid_ruby?("begin\n" + code)
      end

      # Checks if a string of Ruby code comes in the middle of a block.
      # This could be a keyword like `else`, `rescue`, or `when`,
      # or even `end` with a method call that takes a block.
      #
      # @param code [String] Ruby code to check
      # @return [Boolean]
      def mid_block?(code)
        return if valid_ruby?(code)
        valid_ruby?("if foo\n#{code}\nend") || # else, elsif
          valid_ruby?("begin\n#{code}\nend") || # rescue, ensure
          valid_ruby?("case foo\n#{code}\nend") # when
      end
    end
  end
end
