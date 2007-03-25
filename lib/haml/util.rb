# This file contains various useful bits of code
# that are shared between Haml and Sass.

class Hash # :nodoc:
  # Same as Hash#merge!,
  # but recursively merges sub-hashes and arrays
  def rec_merge!(other)
    other.each do |key, value|
      myval = self[key]
      if value.is_a?(Hash) && myval.is_a?(Hash)
        myval.rec_merge!(value)
      elsif value.is_a?(Array) && myval.is_a?(Array)
        myval.concat(value)
      else
        self[key] = value
      end
    end
    self
  end
end
