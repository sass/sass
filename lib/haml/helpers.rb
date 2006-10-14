module Haml
  # This module contains various helpful methods to make it easier to do
  # various tasks. Haml::Helpers is automatically included in the context
  # that a HAML template is parsed in, so all these methods are at your
  # disposal from within the template.
  module Helpers
    # Takes any string, finds all the endlines and converts them to
    # html entities for endlines so they'll render correctly in
    # whitespace-sensitive tags.
    def flatten(input)
      input.gsub(/\n/, '&#x000A;').gsub(/\r/, '')
    end

    # Isolates the whitespace-sensitive tags in the string and uses flatten
    # to convert any endlines inside them into html entities.
    def find_and_flatten(input)
      input.scan(/<(textarea|code|pre)[^>]*>(.*?)<\/\1>/im).each do |thing|
        input = input.gsub(thing[1], flatten(thing[1]))
      end
      input
    end

    # Counts the tabulation of a line. Mostly for internal use.
    def count_soft_tabs(line)
      line.index(/[^ ]/) ? [line.index(/[^ ]/)/2, line.strip] : []
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
  end
end
