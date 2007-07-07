# This file contains various useful bits of code
# that are shared between Haml and Sass.

class Hash # :nodoc:
  # Same as Hash#merge!,
  # but recursively merges sub-hashes
  def rec_merge!(other)
    other.each do |key, value|
      myval = self[key]
      if value.is_a?(Hash) && myval.is_a?(Hash)
        myval.rec_merge!(value)
      else
        self[key] = value
      end
    end
    self
  end
end
