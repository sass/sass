require 'haml/util'

module Haml
  # Handles Haml version-reporting.
  # Haml not only reports the standard three version numbers,
  # but its Git revision hash as well,
  # if it was installed from Git.
  module Version
    include Haml::Util

    # Returns a hash representing the version of Haml.
    # The `:major`, `:minor`, and `:teeny` keys have their respective numbers as Fixnums.
    # The `:string` key contains a human-readable string representation of the version.
    # If Haml is checked out from Git, the `:rev` key will have the revision hash.
    # For example:
    #
    #     {
    #       :string=>"2.1.0.9616393",
    #       :rev => "9616393b8924ef36639c7e82aa88a51a24d16949",
    #       :major => 2, :minor => 1, :teeny => 0
    #     }
    #
    # @return [Hash<Symbol, String/Symbol>] The version hash
    def version
      return @@version if defined?(@@version)

      numbers = File.read(scope('VERSION')).strip.split('.').map { |n| n.to_i }
      @@version = {
        :major => numbers[0],
        :minor => numbers[1],
        :teeny => numbers[2]
      }
      @@version[:string] = [:major, :minor, :teeny].map { |comp| @@version[comp] }.compact.join('.')

      if File.exists?(scope('REVISION'))
        rev = File.read(scope('REVISION')).strip
        rev = nil if rev !~ /^([a-f0-9]+|\(.*\))$/
      end

      if (rev.nil? || rev == '(unknown)') && File.exists?(scope('.git/HEAD'))
        rev = File.read(scope('.git/HEAD')).strip
        if rev =~ /^ref: (.*)$/
          rev = File.read(scope(".git/#{$1}")).strip
        end
      end

      if rev
        @@version[:rev] = rev
        unless rev[0] == ?(
          @@version[:string] << "."
          @@version[:string] << rev[0...7]
        end
      end

      @@version
    end
  end
end
