require 'set'

# Provides `Sass.has_feature?` which allows for simple feature detection
# by providing a feature name.
module Sass
  module Features

    # This is the set of features that can be detected.
    KNOWN_FEATURES = Set[*%w{
      sourcemaps
    }]

    # Check if a feature exists by name. This is used to implement
    # the Sass function `sass-supports($feature)`
    #
    #   @return [Bollean] whether the feature of that name exists.
    def has_feature?(feature_name)
      KNOWN_FEATURES.include?(feature_name)
    end
  end

  extend Features
end


