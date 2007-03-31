begin
  require 'rubygems'
  require 'active_support'
  require 'action_controller'
  require 'action_view'
  action_view_included = true
rescue LoadError
  action_view_included = false
end

if action_view_included  
  module ActionView
    # This overrides various helpers in ActionView
    # to make them work more effectively with Haml.
    module Helpers
      # :stopdoc:
      module TextHelper
        def concat_with_haml(string, binding = nil)
          if is_haml?
            buffer.buffer.concat(string)
          else
            concat_without_haml(string, binding)
          end
        end
        alias_method_chain :concat, :haml
      end

      module FormTagHelper
        def form_tag_with_haml(url_for_options = {}, options = {}, *parameters_for_url, &proc)
          if block_given? && is_haml?
            oldproc = proc 
            proc = bind_proc do |*args|
              concat "\n"
              tab_up
              oldproc.call(*args)
              tab_down
            end
          end
          res = form_tag_without_haml(url_for_options, options, *parameters_for_url, &proc) + "\n"
          concat "\n" if block_given? && is_haml?
          res
        end
        alias_method_chain :form_tag, :haml
      end

      module FormHelper
        def form_for_with_haml(object_name, *args, &proc)
          if block_given? && is_haml?
            oldproc = proc 
            proc = bind_proc do |*args|
              tab_up
              oldproc.call(*args)
              tab_down
            end
          end
          form_for_without_haml(object_name, *args, &proc)
          concat "\n" if block_given? && is_haml?
        end
        alias_method_chain :form_for, :haml
      end
      # :startdoc:
    end
  end
end

