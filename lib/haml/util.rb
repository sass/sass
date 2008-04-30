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

class Object
  # Haml overrides various ActionView helpers,
  # which call an #is_haml? method
  # to determine whether or not the current context object
  # is a proper Haml context.
  # Because ActionView helpers may be included in non-ActionView::Base classes,
  # it's a good idea to define is_haml? for all objects.
  def is_haml?
    false
  end
end
