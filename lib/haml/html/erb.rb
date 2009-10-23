require 'cgi'
require 'erubis'
require 'ruby_parser'

module Haml
  class HTML
    class ERB < Erubis::Basic::Engine
      def self.compile(template)
        new(template).src
      end

      def escaped_expr(code)
        raise "html2haml doesn't support escaped expressions."
      end

      def add_preamble(src); end
      def add_postamble(src); end

      def add_text(src, text)
        src << text
      end

      def add_stmt(src, code)
        src << '</haml:block>' if block_closer?(code) || mid_block?(code)
        src << '<haml:silent>' << h(code) << '</haml:silent>' unless code.strip == "end"
        src << '<haml:block>' if block_opener?(code) || mid_block?(code)
      end

      def add_expr_literal(src, code)
        src << '<haml:loud>' << h(code) << '</haml:loud>'
        src << '<haml:block>' if block_opener?(code)
      end

      def add_expr_debug(src, code)
        raise "html2haml doesn't support debugging expressions."
      end

      private

      def h(code)
        CGI.escapeHTML(code)
      end

      # Returns whether the code is valid Ruby code on its own
      def valid_ruby?(code)
        RubyParser.new.parse(code)
      rescue Racc::ParseError => e
        false
      end

      # Checks if the Ruby code opens a block
      def block_opener?(code)
        valid_ruby?(code + "\nend") ||
          valid_ruby?(code + "\nwhen foo\nend")
      end

      # Checks if the Ruby code closes a block
      def block_closer?(code)
        valid_ruby?("begin\n" + code)
      end

      # Checks if the Ruby code comes in the middle of a block
      def mid_block?(code)
        return if valid_ruby?(code)
        valid_ruby?("if foo\n#{code}\nend") || # else, elsif
          valid_ruby?("begin\n#{code}\nend") || # rescue, ensure
          valid_ruby?("case foo\n#{code}\nend") # when
      end
    end
  end
end
