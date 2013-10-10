module Sass
  module Selector
    # The abstract parent class of the various selector sequence classes.
    #
    # All subclasses should implement a `members` method that returns an array
    # of object that respond to `#line=` and `#filename=`, as well as a `to_a`
    # method that returns an array of strings and script nodes.
    class AbstractSequence
      # The line of the Sass template on which this selector was declared.
      #
      # @return [Fixnum]
      attr_reader :line

      # The name of the file in which this selector was declared.
      #
      # @return [String, nil]
      attr_reader :filename

      # Sets the line of the Sass template on which this selector was declared.
      # This also sets the line for all child selectors.
      #
      # @param line [Fixnum]
      # @return [Fixnum]
      def line=(line)
        members.each {|m| m.line = line}
        @line = line
      end

      # Sets the name of the file in which this selector was declared,
      # or `nil` if it was not declared in a file (e.g. on stdin).
      # This also sets the filename for all child selectors.
      #
      # @param filename [String, nil]
      # @return [String, nil]
      def filename=(filename)
        members.each {|m| m.filename = filename}
        @filename = filename
      end

      # Returns a hash code for this sequence.
      #
      # Subclasses should define `#_hash` rather than overriding this method,
      # which automatically handles memoizing the result.
      #
      # @return [Fixnum]
      def hash
        @_hash ||= _hash
      end

      # Checks equality between this and another object.
      #
      # Subclasses should define `#_eql?` rather than overriding this method,
      # which handles checking class equality and hash equality.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class && other.hash == hash && _eql?(other)
      end
      alias_method :==, :eql?

      # Whether or not this selector sequence contains a placeholder selector.
      # Checks recursively.
      def has_placeholder?
        @has_placeholder ||=
          members.any? {|m| m.is_a?(AbstractSequence) ? m.has_placeholder? : m.is_a?(Placeholder)}
      end

      # Converts the selector into a string. This is the standard selector
      # string, along with any SassScript interpolation that may exist.
      #
      # @return [String]
      def to_s
        to_a.map {|e| e.is_a?(Sass::Script::Tree::Node) ? "\#{#{e.to_sass}}" : e}.join
      end

      # Returns the specificity of the selector as an integer. The base is given
      # by {Sass::Selector::SPECIFICITY_BASE}.
      #
      # @return [Fixnum]
      def specificity
        _specificity(members)
      end

      protected

      def _specificity(arr)
        spec = 0
        arr.map {|m| spec += m.is_a?(String) ? 0 : m.specificity}
        spec
      end
    end
  end
end
