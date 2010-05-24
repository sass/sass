module ActionView
  class Base
    def render_with_haml(*args, &block)
      options = args.first

      # If render :layout is used with a block,
      # it concats rather than returning a string
      # so we need it to keep thinking it's Haml
      # until it hits the sub-render
      if is_haml? && !(options.is_a?(Hash) && options[:layout] && block_given?)
        return non_haml { render_without_haml(*args, &block) }
      end
      render_without_haml(*args, &block)
    end
    alias_method :render_without_haml, :render
    alias_method :render, :render_with_haml

    # Rails >2.1
    if Haml::Util.has?(:instance_method, self, :output_buffer)
      def output_buffer_with_haml
        return haml_buffer.buffer if is_haml?
        output_buffer_without_haml
      end
      alias_method :output_buffer_without_haml, :output_buffer
      alias_method :output_buffer, :output_buffer_with_haml

      def set_output_buffer_with_haml(new)
        if is_haml?
          new = String.new(new) if Haml::Util.rails_xss_safe? &&
            new.is_a?(Haml::Util.rails_safe_buffer_class)
          haml_buffer.buffer = new
        else
          set_output_buffer_without_haml new
        end
      end
      alias_method :set_output_buffer_without_haml, :output_buffer=
      alias_method :output_buffer=, :set_output_buffer_with_haml
    end
  end

  module Helpers
    # In Rails <=2.1, we've got to override considerable capturing infrastructure.
    # In Rails >2.1, we can make do with only overriding #capture
    # (which no longer behaves differently in helper contexts).
    unless Haml::Util.has?(:instance_method, ActionView::Base, :output_buffer)
      module CaptureHelper
        def capture_with_haml(*args, &block)
          # Rails' #capture helper will just return the value of the block
          # if it's not actually in the template context,
          # as detected by the existance of an _erbout variable.
          # We've got to do the same thing for compatibility.

          if is_haml? && block_is_haml?(block)
            capture_haml(*args, &block)
          else
            capture_without_haml(*args, &block)
          end
        end
        alias_method :capture_without_haml, :capture
        alias_method :capture, :capture_with_haml

        def capture_erb_with_buffer_with_haml(buffer, *args, &block)
          if is_haml?
            capture_haml(*args, &block)
          else
            capture_erb_with_buffer_without_haml(buffer, *args, &block)
          end
        end
        alias_method :capture_erb_with_buffer_without_haml, :capture_erb_with_buffer
        alias_method :capture_erb_with_buffer, :capture_erb_with_buffer_with_haml
      end

      module TextHelper
        def concat_with_haml(string, binding = nil)
          if is_haml?
            haml_buffer.buffer.concat(string)
          else
            concat_without_haml(string, binding)
          end
        end
        alias_method :concat_without_haml, :concat
        alias_method :concat, :concat_with_haml
      end
    else
      module CaptureHelper
        def capture_with_haml(*args, &block)
          if Haml::Helpers.block_is_haml?(block)
            str = capture_haml(*args, &block)
            return ActionView::NonConcattingString.new(str) if defined?(ActionView::NonConcattingString)
            return str
          else
            capture_without_haml(*args, &block)
          end
        end
        alias_method :capture_without_haml, :capture
        alias_method :capture, :capture_with_haml
      end
    end

    module TagHelper
      def content_tag_with_haml(name, *args, &block)
        return content_tag_without_haml(name, *args, &block) unless is_haml?

        preserve = haml_buffer.options[:preserve].include?(name.to_s)

        if block_given? && block_is_haml?(block) && preserve
          return content_tag_without_haml(name, *args) {preserve(&block)}
        end

        returning content_tag_without_haml(name, *args, &block) do |content|
          return Haml::Helpers.preserve(content) if preserve && content
        end
      end

      alias_method :content_tag_without_haml, :content_tag
      alias_method :content_tag, :content_tag_with_haml
    end

    class InstanceTag
      # Includes TagHelper

      def haml_buffer
        @template_object.send :haml_buffer
      end

      def is_haml?
        @template_object.send :is_haml?
      end

      def content_tag(*args)
        html_tag = content_tag_with_haml(*args)
        return html_tag unless respond_to?(:error_wrapping)
        return error_wrapping(html_tag) if method(:error_wrapping).arity == 1
        return html_tag unless object.respond_to?(:errors) && object.errors.respond_to?(:on)
        return error_wrapping(html_tag, object.errors.on(@method_name))
      end
    end

    if Haml::Util.ap_geq_3?
      module FormTagHelper
        def form_tag_with_haml(url_for_options = {}, options = {}, *parameters_for_url, &proc)
          if is_haml?
            if block_given?
              oldproc = proc
              proc = haml_bind_proc do |*args|
                concat "\n"
                with_tabs(1) {oldproc.call(*args)}
              end
            end
            res = form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc) + "\n"
            res << "\n" if block_given?
            res
          else
            form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc)
          end
        end
        alias_method :form_tag_without_haml, :form_tag
        alias_method :form_tag, :form_tag_with_haml
      end

      module FormHelper
        def form_for_with_haml(object_name, *args, &proc)
          if block_given? && is_haml?
            oldproc = proc
            proc = proc {|*args| with_tabs(1) {oldproc.call(*args)}}
          end
          res = form_for_without_haml(object_name, *args, &proc)
          res << "\n" if block_given? && is_haml?
          res
        end
        alias_method :form_for_without_haml, :form_for
        alias_method :form_for, :form_for_with_haml
      end

      module CacheHelper
        # This is a workaround for a Rails 3 bug
        # that's present at least through beta 3.
        # Their fragment_for assumes that the block
        # will return its contents as a string,
        # which is not always the case.
        # Luckily, it only makes this assumption if caching is disabled,
        # so we only override that case.
        def fragment_for_with_haml(*args, &block)
          return fragment_for_without_haml(*args, &block) if controller.perform_caching
          capture(&block)
        end
        alias_method :fragment_for_without_haml, :fragment_for
        alias_method :fragment_for, :fragment_for_with_haml
      end
    else
      module FormTagHelper
        def form_tag_with_haml(url_for_options = {}, options = {}, *parameters_for_url, &proc)
          if is_haml?
            if block_given?
              oldproc = proc
              proc = haml_bind_proc do |*args|
                concat "\n"
                tab_up
                oldproc.call(*args)
                tab_down
                concat haml_indent
              end
              concat haml_indent
            end
            res = form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc) + "\n"
            if block_given?
              concat "\n"
              return Haml::Helpers::ErrorReturn.new("form_tag")
            end
            res
          else
            form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc)
          end
        end
        alias_method :form_tag_without_haml, :form_tag
        alias_method :form_tag, :form_tag_with_haml
      end

      module FormHelper
        def form_for_with_haml(object_name, *args, &proc)
          if block_given? && is_haml?
            oldproc = proc
            proc = haml_bind_proc do |*args|
              tab_up
              oldproc.call(*args)
              tab_down
              concat haml_indent
            end
            concat haml_indent
          end
          form_for_without_haml(object_name, *args, &proc)
          concat "\n" if block_given? && is_haml?
          Haml::Helpers::ErrorReturn.new("form_for") if is_haml?
        end
        alias_method :form_for_without_haml, :form_for
        alias_method :form_for, :form_for_with_haml
      end
    end
  end
end
