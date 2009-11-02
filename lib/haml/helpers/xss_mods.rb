module Haml
  module Helpers
    # This module overrides Haml helpers to work properly
    # in the context of ActionView.
    # Currently it's only used for modifying the helpers
    # to work with Rails' XSS protection methods.
    module XssMods
      def self.included(base)
        %w[html_escape find_and_preserve preserve list_of surround
           precede succeed capture_haml haml_concat haml_indent
           haml_tag escape_once].each do |name|
          base.send(:alias_method, "#{name}_without_haml_xss", name)
          base.send(:alias_method, name, "#{name}_with_haml_xss")
        end
      end

      # Don't escape text that's already safe,
      # output is always HTML safe
      def html_escape_with_haml_xss(text)
        str = text.to_s
        return text if str.html_safe?
        html_escape_without_haml_xss(str).html_safe!
      end

      # Output is always HTML safe
      def find_and_preserve_with_haml_xss(*args, &block)
        find_and_preserve_without_haml_xss(*args, &block).html_safe!
      end

      # Output is always HTML safe
      def preserve_with_haml_xss(*args, &block)
        preserve_without_haml_xss(*args, &block).html_safe!
      end

      # Output is always HTML safe
      def list_of_with_haml_xss(*args, &block)
        list_of_without_haml_xss(*args, &block).html_safe!
      end

      # Input is escaped, output is always HTML safe
      def surround_with_haml_xss(front, back = front, &block)
        surround_without_haml_xss(
          haml_xss_html_escape(front),
          haml_xss_html_escape(back),
          &block).html_safe!
      end

      # Input is escaped, output is always HTML safe
      def precede_with_haml_xss(str, &block)
        precede_without_haml_xss(haml_xss_html_escape(str), &block).html_safe!
      end

      # Input is escaped, output is always HTML safe
      def succeed_with_haml_xss(str, &block)
        succeed_without_haml_xss(haml_xss_html_escape(str), &block).html_safe!
      end

      # Output is always HTML safe
      def capture_haml_with_haml_xss(*args, &block)
        capture_haml_without_haml_xss(*args, &block).html_safe!
      end

      # Input is escaped
      def haml_concat_with_haml_xss(text = "")
        haml_concat_without_haml_xss(@_haml_concat_raw ? text : haml_xss_html_escape(text))
      end

      # Output is always HTML safe
      def haml_indent_with_haml_xss
        haml_indent_without_haml_xss.html_safe!
      end

      # Input is escaped, haml_concat'ed output is always HTML safe
      def haml_tag_with_haml_xss(name, *rest, &block)
        name = haml_xss_html_escape(name.to_s)
        rest.unshift(haml_xss_html_escape(rest.shift.to_s)) unless [Symbol, Hash, NilClass].any? {|t| rest.first.is_a? t}
        with_raw_haml_concat {haml_tag_without_haml_xss(name, *rest, &block)}
      end

      # Output is always HTML safe
      def escape_once_with_haml_xss(*args)
        escape_once_without_haml_xss(*args).html_safe!
      end

      private

      # Escapes the HTML in the text if and only if
      # Rails XSS protection is enabled *and* the `:escape_html` option is set.
      def haml_xss_html_escape(text)
        return text unless Haml::Util.rails_xss_safe? && haml_buffer.options[:escape_html]
        html_escape(text)
      end
    end
  end
end
