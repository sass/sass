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
        Haml::Util.html_safe(html_escape_without_haml_xss(str))
      end

      # Output is always HTML safe
      def find_and_preserve_with_haml_xss(*args, &block)
        Haml::Util.html_safe(find_and_preserve_without_haml_xss(*args, &block))
      end

      # Output is always HTML safe
      def preserve_with_haml_xss(*args, &block)
        Haml::Util.html_safe(preserve_without_haml_xss(*args, &block))
      end

      # Output is always HTML safe
      def list_of_with_haml_xss(*args, &block)
        Haml::Util.html_safe(list_of_without_haml_xss(*args, &block))
      end

      # Input is escaped, output is always HTML safe
      def surround_with_haml_xss(front, back = front, &block)
        Haml::Util.html_safe(
          surround_without_haml_xss(
            haml_xss_html_escape(front),
            haml_xss_html_escape(back),
            &block))
      end

      # Input is escaped, output is always HTML safe
      def precede_with_haml_xss(str, &block)
        Haml::Util.html_safe(precede_without_haml_xss(haml_xss_html_escape(str), &block))
      end

      # Input is escaped, output is always HTML safe
      def succeed_with_haml_xss(str, &block)
        Haml::Util.html_safe(succeed_without_haml_xss(haml_xss_html_escape(str), &block))
      end

      # Output is always HTML safe
      def capture_haml_with_haml_xss(*args, &block)
        Haml::Util.html_safe(capture_haml_without_haml_xss(*args, &block))
      end

      # Input is escaped
      def haml_concat_with_haml_xss(text = "")
        haml_concat_without_haml_xss(@_haml_concat_raw ? text : haml_xss_html_escape(text))
      end

      # Output is always HTML safe
      def haml_indent_with_haml_xss
        Haml::Util.html_safe(haml_indent_without_haml_xss)
      end

      # Input is escaped, haml_concat'ed output is always HTML safe
      def haml_tag_with_haml_xss(name, *rest, &block)
        name = haml_xss_html_escape(name.to_s)
        rest.unshift(haml_xss_html_escape(rest.shift.to_s)) unless [Symbol, Hash, NilClass].any? {|t| rest.first.is_a? t}
        with_raw_haml_concat {haml_tag_without_haml_xss(name, *rest, &block)}
      end

      # Output is always HTML safe
      def escape_once_with_haml_xss(*args)
        Haml::Util.html_safe(escape_once_without_haml_xss(*args))
      end

      private

      # Escapes the HTML in the text if and only if
      # Rails XSS protection is enabled *and* the `:escape_html` option is set.
      def haml_xss_html_escape(text)
        return text unless Haml::Util.rails_xss_safe? && haml_buffer.options[:escape_html]
        html_escape(text)
      end
    end

    class ErrorReturn
      # Any attempt to treat ErrorReturn as a string should cause it to blow up.
      alias_method :html_safe, :to_s
      alias_method :html_safe?, :to_s
      alias_method :html_safe!, :to_s
    end
  end
end

module ActionView
  module Helpers
    module CaptureHelper
      def with_output_buffer_with_haml_xss(*args, &block)
        res = with_output_buffer_without_haml_xss(*args, &block)
        case res
        when Array; res.map {|s| Haml::Util.html_safe(s)}
        when String; Haml::Util.html_safe(res)
        else; res
        end
      end
      alias_method :with_output_buffer_without_haml_xss, :with_output_buffer
      alias_method :with_output_buffer, :with_output_buffer_with_haml_xss
    end

    module FormTagHelper
      def form_tag_with_haml_xss(*args, &block)
        res = form_tag_without_haml_xss(*args, &block)
        res = Haml::Util.html_safe(res) unless block_given?
        res
      end
      alias_method :form_tag_without_haml_xss, :form_tag
      alias_method :form_tag, :form_tag_with_haml_xss
    end

    module FormHelper
      def form_for_with_haml_xss(*args, &block)
        res = form_for_without_haml_xss(*args, &block)
        return Haml::Util.html_safe(res) if res.is_a?(String)
        return res
      end
      alias_method :form_for_without_haml_xss, :form_for
      alias_method :form_for, :form_for_with_haml_xss
    end

    module TextHelper
      def concat_with_haml_xss(string)
        if is_haml?
          haml_buffer.buffer.concat(haml_xss_html_escape(string))
        else
          concat_without_haml_xss(string)
        end
      end
      alias_method :concat_without_haml_xss, :concat
      alias_method :concat, :concat_with_haml_xss

      # safe_concat was introduced in Rails 3.0
      if Haml::Util.has?(:instance_method, self, :safe_concat)
        def safe_concat_with_haml_xss(string)
          if is_haml?
            haml_buffer.buffer.concat(string)
          else
            safe_concat_without_haml_xss(string)
          end
        end
        alias_method :safe_concat_without_haml_xss, :safe_concat
        alias_method :safe_concat, :safe_concat_with_haml_xss
      end
    end
  end
end
