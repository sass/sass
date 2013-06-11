require 'set'

# Provides `Sass.has_feature?` which allows for simple feature detection
# by providing a feature name.
module Sass
  module Features

    # This is the set of features that can be detected.
    KNOWN_FEATURES = Set[*%w{
    }]

    # Check if a feature exists by name. This is used to implement
    # the Sass function `sass-supports($feature)`
    #
    # @param feature_name [String] The case sensitive name of the feature to
    #        check if it exists in this version of Sass.
    #
    # @return [Boolean] whether the feature of that name exists.
    def has_feature?(feature_name)
      KNOWN_FEATURES.include?(feature_name)
    end
  end

  extend Features
end


