module Sass
  module SCSS
    # A subclass of {Parser} that parses code in Sass documents
    # using some SCSS constructs.
    # This is necessary because SassScript in Sass supports `!`-style variables,
    # whereas in SCSS it doesn't.
    class SassParser < Parser
      @sass_script_parser = Sass::Script::Parser
    end
  end
end
