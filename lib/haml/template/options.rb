# We keep options in its own self-contained file
# so that we can load it independently in Rails 3,
# where the full template stuff is lazy-loaded.

module Haml
  module Template
    extend self

    @options = {}
    # The options hash for Haml when used within Rails.
    # See {file:HAML_REFERENCE.md#haml_options the Haml options documentation}.
    #
    # @return [{Symbol => Object}]
    attr_accessor :options
  end
end
