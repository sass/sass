require File.dirname(__FILE__) + '/helpers/action_view_mods'

module Haml
  # This module contains various helpful methods to make it easier to do
  # various tasks. Haml::Helpers is automatically included in the context
  # that a HAML template is parsed in, so all these methods are at your
  # disposal from within the template.
  module Helpers
    self.extend self
    
    @@action_view = false
    @@force_no_action_view = false

    # Returns whether or not ActionView is available.
    def self.action_view?
      @@action_view
    end
    
    # Sets whether or not ActionView is available.
    def self.action_view(value) # :nodoc:
      @@action_view = value
    end

    # Takes any string, finds all the endlines and converts them to
    # html entities for endlines so they'll render correctly in
    # whitespace-sensitive tags.
    def flatten(input)
      input.gsub(/\n/, '&#x000A;').gsub(/\r/, '')
    end

    # Takes an array and a block and iterates the array,
    # yielding each element to the block and putting the
    # result into <tt>li</tt> elements, creating a list
    # of the results of the block. For example:
    #
    # For instance:
    #   list_of([['hello'], ['yall']]) { |i| i[0] }
    # or
    #   list_of(['hello', 'yall'])
    #
    # Produces:
    #   <li>hello</li>
    #   <li>yall</li>
    #
    def list_of(array) # :yields: item
      (array.collect { |i| "<li>#{yield(i)}</li>" }).join("\n")
    end

    # Increments the tabulation modifier of the buffer. This is the
    # number of tabs the buffer automatically adds to the lines of the
    # template.
    def tab_up(i = 1)
      buffer.tabulation += i
    end

    # Decrements the tabulation modifier of the buffer. This is the
    # number of tabs the buffer automatically adds to the lines of the
    # template.
    def tab_down(i = 1)
      buffer.tabulation -= i
    end

    # Gets a reference to the current Haml::Buffer object.
    def buffer # :nodoc:
      @haml_stack[-1]
    end
    
    # Gives a proc the same local "_hamlout" and "_erbout" variables
    # that the current template has.
    def bind_proc(&proc)
      _hamlout = buffer
      _erbout = _hamlout.buffer
      proc { |*args| proc.call(*args) }
    end
    
    include ActionViewMods if self.const_defined?  "ActionViewMods"
  end
end
