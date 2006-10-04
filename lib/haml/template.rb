require File.dirname(__FILE__) + '/engine'

module Haml
  # This is the class that is registered as the template handler for
  # ActionView. Beyond fitting into that interface, it has little use.
  class Template
    # Creates a new Template class using the given ActionView::Base instance.
    def initialize(view)
      @view = view
    end
    
    # Assigns the variables in the <tt>local_assigns</tt> hash, renders the given
    # template, and returns the result.
    def render(template, local_assigns={})
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
          class << self; self; end.send(:define_method, key) { val }
        end
      end
      
      Haml::Engine.new(template, :scope_object => @view).to_html
    end
  end
end