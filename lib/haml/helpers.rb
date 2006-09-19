module Haml
  module Helpers
    # Flatten will take any string, find all the endlines (via \n)
    # and convert them to html entities for endlines.
    def flatten(input)
      input.gsub(/\n/, '&#x000A;').gsub(/\r/, '')
    end

    def find_and_flatten(input)
      input.scan(/<(textarea|code|pre)[^>]*>(.*?)<\/\1>/im).each do |thing|
        input = input.gsub(thing[1], flatten(thing[1]))
      end
      input
    end

    def tabs(count)
      '  ' * count
    end

    def count_soft_tabs(line)
      line.index(/[^ ]/) ? [line.index(/[^ ]/)/2, line.strip] : []
    end

    # List_for is a really nifty little helper that helps
    # cleanup your code. Basically, give it an array of
    # objects, and then pass in a block that tells how
    # what to put out, and you will get each block item
    # in rows of <li> tags.
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
    def list_of(array)
      (array.collect { |i| "<li>#{yield(i)}</li>" }).join("\n")
    end
  end
end
