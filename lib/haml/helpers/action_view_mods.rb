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
  class ActionView::Base
    alias_method :old_concat, :concat unless instance_methods.include? "old_concat"
    alias_method :old_form_tag, :form_tag unless instance_methods.include? "old_form_tag"

    alias_method :old_form_for, :form_for unless instance_methods.include? "old_form_for"
  end
  
  module Haml
    module Helpers
      # This module overrides various helpers in ActionView
      # to make them work more effectively with Haml.
      # It also defines several helper methods,
      # available from a Haml template,
      # which are only useful within the context of ActionView.
      # It's not available unless ActionView is installed.
      #
      #--
      # Methods in this module should be nodoc'd.
      #++
      module ActionViewMods
        def self.included(othermod) # :nodoc:
          othermod.class_eval do
            action_view(true)
            alias_method :capture_erb_with_buffer, :capture_haml_with_buffer
          end
        end
        
        def concat(string, binding = nil) # :nodoc:
          if is_haml?
            buffer.buffer.concat(string)
          else
            old_concat(string, binding)
          end
        end
        
        def form_tag(url_for_options = {}, options = {}, *parameters_for_url, &proc) # :nodoc:
          if block_given? && is_haml?
            oldproc = proc 
            proc = bind_proc do |*args|
              concat "\n"
              tab_up
              oldproc.call(*args)
              tab_down
            end
          end
          res = old_form_tag(url_for_options, options, *parameters_for_url, &proc) + "\n"
          concat "\n" if block_given? && is_haml?
          res
        end

        # View accessor for the push_text helper
        def push_text(text, tabulation = 0)
          buffer.push_text(text, tabulation)
        end

        # open_tag helps you construct HTML in your helpers.
        # It can be used this way
        #
        #   open_tag :table do
        #     open_tag :tr do
        #       open_tag :td do
        #         push_text "data"
        #       end
        #       open_tag :td do
        #         push_text "more_data"
        #       end
        #     end
        #   end
        #
        # TODO: Make it output with better tabulation
        # TODO: TEST!!!!
        def open_tag(named, text = nil, options = {}, &block)
          # TODO: I'm sure re-coding this is bad. I know we do this elsewhere, obviously.
          attributes = (options.collect { |key, value| "#{key}='#{value}'" }).join(" ")
          push_text "<#{named}#{attributes}>"
          tab_up
          text || block.call
          concat "\n"
          tab_down
          push_text "</#{named}>"
        end

        def form_for(object_name, *args, &proc) # :nodoc:
          if block_given? && is_haml?
            oldproc = proc 
            proc = bind_proc do |*args|
              tab_up
              oldproc.call(*args)
              tab_down
            end
          end
          old_form_for(object_name, *args, &proc)
          concat "\n" if block_given? && is_haml?
        end

        def generate_content_class_names
          controller.controller_name + " " + controller.action_name
        end
      end
    end
  end
end

