if defined?(ActionView) and not defined?(Merb::Plugins)
  module ActionView
    class Base # :nodoc:
      def render_with_haml(*args, &block)
        was_haml = is_haml?
        @haml_is_haml = false
        res = render_without_haml(*args, &block)
        @haml_is_haml = was_haml
        res
      end
      alias_method :render_without_haml, :render
      alias_method :render, :render_with_haml
    end

    # This overrides various helpers in ActionView
    # to make them work more effectively with Haml.
    module Helpers
      # :stopdoc:
      module CaptureHelper
        def capture_erb_with_buffer_with_haml(*args, &block)
          if is_haml?
            capture_haml_with_buffer(*args, &block)
          else
            capture_erb_with_buffer_without_haml(*args, &block)
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
              end
            end
            res = form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc) + "\n"
            concat "\n" if block_given? && is_haml?
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
            end
          end
          form_for_without_haml(object_name, *args, &proc)
          concat "\n" if block_given? && is_haml?
        end
        alias_method :form_for_without_haml, :form_for
        alias_method :form_for, :form_for_with_haml
      end
      # :startdoc:
    end
  end
end

