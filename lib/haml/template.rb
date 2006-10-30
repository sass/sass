require File.dirname(__FILE__) + '/engine'
require 'active_support'
require 'action_view'

module Haml
  class Template
  
    class << self
      @@options = {}
      
      # Gets various options for HAML. See REFERENCE for details.
      def options
        @@options
      end
      
      # Sets various options for HAML. See REFERENCE for details.
      def options=(value)
        @@options = value
      end
    end

    def initialize(view)
      @view = view
      @@precompiled_templates ||= {}
    end

    def render(template_file_name, local_assigns={})
      assigns = @view.assigns.dup

      # Do content for layout on its own to keep things working in partials
      if content_for_layout = @view.instance_variable_get("@content_for_layout")
        assigns['content_for_layout'] = content_for_layout
      end

      # Get inside the view object's world
      @view.instance_eval do
        # Set all the instance variables
        assigns.each do |key,val|
          instance_variable_set "@#{key}", val
        end
        # Set all the local assigns
        local_assigns.each do |key,val|
          self.class.send(:define_method, key) { val }
        end
      end
      
      if @precompiled = get_precompiled(template_file_name)
        options = { :precompiled => @precompiled }.merge @@options
        engine = Haml::Engine.new("", options)
      else
        engine = Haml::Engine.new(File.read(template_file_name), @@options)
        set_precompiled(template_file_name, engine.precompiled)
      end
      
      yield_proc = @view.instance_eval do
        proc { |*name| instance_variable_get("@content_for_#{name.first || 'layout'}") }
      end

      engine.to_html(@view) { |*args| yield_proc.call(*args) }

    end

    def get_precompiled(filename)
      # Do we have it on file? Is it new enough?
      if (precompiled, precompiled_on = @@precompiled_templates[filename]) &&
             (precompiled_on == File.mtime(filename).to_i)
        precompiled
      end
    end

    def set_precompiled(filename, precompiled)
      @@precompiled_templates[filename] = [precompiled, File.mtime(filename).to_i]
    end
  end
end

class ActionView::Base
  attr :haml_filename, true

  alias_method :haml_old_render_file, :render_file
  def render_file(template_path, use_full_path = true, local_assigns = {})
    @haml_filename = File.basename(template_path)
    haml_old_render_file(template_path, use_full_path, local_assigns)
  end

  alias_method :read_template_file_old, :read_template_file
  def read_template_file(template_path, extension)
    if extension =~ /haml/i
      template_path
    else
      read_template_file_old(template_path, extension)
    end
  end
end
