require 'erb'
require 'set'
require 'enumerator'
require 'stringio'
require 'rbconfig'
require 'uri'
require 'thread'
require 'pathname'

require 'sass/root'
require 'sass/util/subset_map'

module Sass
  # A module containing various useful functions.
  module Util
    extend self

    # An array of ints representing the Ruby version number.
    # @api public
    RUBY_VERSION = ::RUBY_VERSION.split(".").map {|s| s.to_i}

    # The Ruby engine we're running under. Defaults to `"ruby"`
    # if the top-level constant is undefined.
    # @api public
    RUBY_ENGINE = defined?(::RUBY_ENGINE) ? ::RUBY_ENGINE : "ruby"

    # Returns the path of a file relative to the Sass root directory.
    #
    # @param file [String] The filename relative to the Sass root
    # @return [String] The filename relative to the the working directory
    def scope(file)
      File.join(Sass::ROOT_DIR, file)
    end

    # Converts an array of `[key, value]` pairs to a hash.
    #
    # @example
    #   to_hash([[:foo, "bar"], [:baz, "bang"]])
    #     #=> {:foo => "bar", :baz => "bang"}
    # @param arr [Array<(Object, Object)>] An array of pairs
    # @return [Hash] A hash
    def to_hash(arr)
      ordered_hash(*arr.compact)
    end

    # Maps the keys in a hash according to a block.
    #
    # @example
    #   map_keys({:foo => "bar", :baz => "bang"}) {|k| k.to_s}
    #     #=> {"foo" => "bar", "baz" => "bang"}
    # @param hash [Hash] The hash to map
    # @yield [key] A block in which the keys are transformed
    # @yieldparam key [Object] The key that should be mapped
    # @yieldreturn [Object] The new value for the key
    # @return [Hash] The mapped hash
    # @see #map_vals
    # @see #map_hash
    def map_keys(hash)
      map_hash(hash) {|k, v| [yield(k), v]}
    end

    # Maps the values in a hash according to a block.
    #
    # @example
    #   map_values({:foo => "bar", :baz => "bang"}) {|v| v.to_sym}
    #     #=> {:foo => :bar, :baz => :bang}
    # @param hash [Hash] The hash to map
    # @yield [value] A block in which the values are transformed
    # @yieldparam value [Object] The value that should be mapped
    # @yieldreturn [Object] The new value for the value
    # @return [Hash] The mapped hash
    # @see #map_keys
    # @see #map_hash
    def map_vals(hash)
      # We don't delegate to map_hash for performance here
      # because map_hash does more than is necessary.
      rv = hash.class.new
      hash = hash.as_stored if hash.is_a?(NormalizedMap)
      hash.each do |k, v|
        rv[k] = yield(v)
      end
      rv
    end

    # Maps the key-value pairs of a hash according to a block.
    #
    # @example
    #   map_hash({:foo => "bar", :baz => "bang"}) {|k, v| [k.to_s, v.to_sym]}
    #     #=> {"foo" => :bar, "baz" => :bang}
    # @param hash [Hash] The hash to map
    # @yield [key, value] A block in which the key-value pairs are transformed
    # @yieldparam [key] The hash key
    # @yieldparam [value] The hash value
    # @yieldreturn [(Object, Object)] The new value for the `[key, value]` pair
    # @return [Hash] The mapped hash
    # @see #map_keys
    # @see #map_vals
    def map_hash(hash)
      # Copy and modify is more performant than mapping to an array and using
      # to_hash on the result.
      rv = hash.class.new
      hash.each do |k, v|
        new_key, new_value = yield(k, v)
        new_key = hash.denormalize(new_key) if hash.is_a?(NormalizedMap) && new_key == k
        rv[new_key] = new_value
      end
      rv
    end

    # Computes the powerset of the given array.
    # This is the set of all subsets of the array.
    #
    # @example
    #   powerset([1, 2, 3]) #=>
    #     Set[Set[], Set[1], Set[2], Set[3], Set[1, 2], Set[2, 3], Set[1, 3], Set[1, 2, 3]]
    # @param arr [Enumerable]
    # @return [Set<Set>] The subsets of `arr`
    def powerset(arr)
      arr.inject([Set.new].to_set) do |powerset, el|
        new_powerset = Set.new
        powerset.each do |subset|
          new_powerset << subset
          new_powerset << subset + [el]
        end
        new_powerset
      end
    end

    # Restricts a number to falling within a given range.
    # Returns the number if it falls within the range,
    # or the closest value in the range if it doesn't.
    #
    # @param value [Numeric]
    # @param range [Range<Numeric>]
    # @return [Numeric]
    def restrict(value, range)
      [[value, range.first].max, range.last].min
    end

    # Concatenates all strings that are adjacent in an array,
    # while leaving other elements as they are.
    #
    # @example
    #   merge_adjacent_strings([1, "foo", "bar", 2, "baz"])
    #     #=> [1, "foobar", 2, "baz"]
    # @param arr [Array]
    # @return [Array] The enumerable with strings merged
    def merge_adjacent_strings(arr)
      # Optimize for the common case of one element
      return arr if arr.size < 2
      arr.inject([]) do |a, e|
        if e.is_a?(String)
          if a.last.is_a?(String)
            a.last << e
          else
            a << e.dup
          end
        else
          a << e
        end
        a
      end
    end

    # Non-destructively replaces all occurrences of a subsequence in an array
    # with another subsequence.
    #
    # @example
    #   replace_subseq([1, 2, 3, 4, 5], [2, 3], [:a, :b])
    #     #=> [1, :a, :b, 4, 5]
    #
    # @param arr [Array] The array whose subsequences will be replaced.
    # @param subseq [Array] The subsequence to find and replace.
    # @param replacement [Array] The sequence that `subseq` will be replaced with.
    # @return [Array] `arr` with `subseq` replaced with `replacement`.
    def replace_subseq(arr, subseq, replacement)
      new = []
      matched = []
      i = 0
      arr.each do |elem|
        if elem != subseq[i]
          new.push(*matched)
          matched = []
          i = 0
          new << elem
          next
        end

        if i == subseq.length - 1
          matched = []
          i = 0
          new.push(*replacement)
        else
          matched << elem
          i += 1
        end
      end
      new.push(*matched)
      new
    end

    # Intersperses a value in an enumerable, as would be done with `Array#join`
    # but without concatenating the array together afterwards.
    #
    # @param enum [Enumerable]
    # @param val
    # @return [Array]
    def intersperse(enum, val)
      enum.inject([]) {|a, e| a << e << val}[0...-1]
    end

    def slice_by(enum)
      results = []
      enum.each do |value|
        key = yield(value)
        if !results.empty? && results.last.first == key
          results.last.last << value
        else
          results << [key, [value]]
        end
      end
      results
    end

    # Substitutes a sub-array of one array with another sub-array.
    #
    # @param ary [Array] The array in which to make the substitution
    # @param from [Array] The sequence of elements to replace with `to`
    # @param to [Array] The sequence of elements to replace `from` with
    def substitute(ary, from, to)
      res = ary.dup
      i = 0
      while i < res.size
        if res[i...i + from.size] == from
          res[i...i + from.size] = to
        end
        i += 1
      end
      res
    end

    # Destructively strips whitespace from the beginning and end
    # of the first and last elements, respectively,
    # in the array (if those elements are strings).
    #
    # @param arr [Array]
    # @return [Array] `arr`
    def strip_string_array(arr)
      arr.first.lstrip! if arr.first.is_a?(String)
      arr.last.rstrip! if arr.last.is_a?(String)
      arr
    end

    # Return an array of all possible paths through the given arrays.
    #
    # @param arrs [Array<Array>]
    # @return [Array<Arrays>]
    #
    # @example
    #   paths([[1, 2], [3, 4], [5]]) #=>
    #     # [[1, 3, 5],
    #     #  [2, 3, 5],
    #     #  [1, 4, 5],
    #     #  [2, 4, 5]]
    def paths(arrs)
      arrs.inject([[]]) do |paths, arr|
        flatten(arr.map {|e| paths.map {|path| path + [e]}}, 1)
      end
    end

    # Computes a single longest common subsequence for `x` and `y`.
    # If there are more than one longest common subsequences,
    # the one returned is that which starts first in `x`.
    #
    # @param x [Array]
    # @param y [Array]
    # @yield [a, b] An optional block to use in place of a check for equality
    #   between elements of `x` and `y`.
    # @yieldreturn [Object, nil] If the two values register as equal,
    #   this will return the value to use in the LCS array.
    # @return [Array] The LCS
    def lcs(x, y, &block)
      x = [nil, *x]
      y = [nil, *y]
      block ||= proc {|a, b| a == b && a}
      lcs_backtrace(lcs_table(x, y, &block), x, y, x.size - 1, y.size - 1, &block)
    end

    # Converts a Hash to an Array. This is usually identical to `Hash#to_a`,
    # with the following exceptions:
    #
    # * In Ruby 1.8, `Hash#to_a` is not deterministically ordered, but this is.
    # * In Ruby 1.9 when running tests, this is ordered in the same way it would
    #   be under Ruby 1.8 (sorted key order rather than insertion order).
    #
    # @param hash [Hash]
    # @return [Array]
    def hash_to_a(hash)
      return hash.to_a unless ruby1_8? || defined?(Test::Unit)
      hash.sort_by {|k, v| k}
    end

    # Performs the equivalent of `enum.group_by.to_a`, but with a guaranteed
    # order. Unlike {Util#hash_to_a}, the resulting order isn't sorted key order;
    # instead, it's the same order as `#group_by` has under Ruby 1.9 (key
    # appearance order).
    #
    # @param enum [Enumerable]
    # @return [Array<[Object, Array]>] An array of pairs.
    def group_by_to_a(enum)
      return enum.group_by {|e| yield(e)}.to_a unless ruby1_8?
      order = {}
      arr = []
      groups = enum.group_by do |e|
        res = yield(e)
        unless order.include?(res)
          order[res] = order.size
        end
        res
      end
      groups.each do |key, vals|
        arr[order[key]] = [key, vals]
      end
      arr
    end

    # Returns a sub-array of `minuend` containing only elements that are also in
    # `subtrahend`. Ensures that the return value has the same order as
    # `minuend`, even on Rubinius where that's not guaranteed by `Array#-`.
    #
    # @param minuend [Array]
    # @param subtrahend [Array]
    # @return [Array]
    def array_minus(minuend, subtrahend)
      return minuend - subtrahend unless rbx?
      set = Set.new(minuend) - subtrahend
      minuend.select {|e| set.include?(e)}
    end

    # Returns a string description of the character that caused an
    # `Encoding::UndefinedConversionError`.
    #
    # @param e [Encoding::UndefinedConversionError]
    # @return [String]
    def undefined_conversion_error_char(e)
      # Rubinius (as of 2.0.0.rc1) pre-quotes the error character.
      return e.error_char if rbx?
      # JRuby (as of 1.7.2) doesn't have an error_char field on
      # Encoding::UndefinedConversionError.
      return e.error_char.dump unless jruby?
      e.message[/^"[^"]+"/] # "
    end

    # Asserts that `value` falls within `range` (inclusive), leaving
    # room for slight floating-point errors.
    #
    # @param name [String] The name of the value. Used in the error message.
    # @param range [Range] The allowed range of values.
    # @param value [Numeric, Sass::Script::Value::Number] The value to check.
    # @param unit [String] The unit of the value. Used in error reporting.
    # @return [Numeric] `value` adjusted to fall within range, if it
    #   was outside by a floating-point margin.
    def check_range(name, range, value, unit = '')
      grace = (-0.00001..0.00001)
      str = value.to_s
      value = value.value if value.is_a?(Sass::Script::Value::Number)
      return value if range.include?(value)
      return range.first if grace.include?(value - range.first)
      return range.last if grace.include?(value - range.last)
      raise ArgumentError.new(
        "#{name} #{str} must be between #{range.first}#{unit} and #{range.last}#{unit}")
    end

    # Returns whether or not `seq1` is a subsequence of `seq2`. That is, whether
    # or not `seq2` contains every element in `seq1` in the same order (and
    # possibly more elements besides).
    #
    # @param seq1 [Array]
    # @param seq2 [Array]
    # @return [Boolean]
    def subsequence?(seq1, seq2)
      i = j = 0
      loop do
        return true if i == seq1.size
        return false if j == seq2.size
        i += 1 if seq1[i] == seq2[j]
        j += 1
      end
    end

    # Returns information about the caller of the previous method.
    #
    # @param entry [String] An entry in the `#caller` list, or a similarly formatted string
    # @return [[String, Fixnum, (String, nil)]]
    #   An array containing the filename, line, and method name of the caller.
    #   The method name may be nil
    def caller_info(entry = nil)
      # JRuby evaluates `caller` incorrectly when it's in an actual default argument.
      entry ||= caller[1]
      info = entry.scan(/^(.*?):(-?.*?)(?::.*`(.+)')?$/).first
      info[1] = info[1].to_i
      # This is added by Rubinius to designate a block, but we don't care about it.
      info[2].sub!(/ \{\}\Z/, '') if info[2]
      info
    end

    # Returns whether one version string represents a more recent version than another.
    #
    # @param v1 [String] A version string.
    # @param v2 [String] Another version string.
    # @return [Boolean]
    def version_gt(v1, v2)
      # Construct an array to make sure the shorter version is padded with nil
      Array.new([v1.length, v2.length].max).zip(v1.split("."), v2.split(".")) do |_, p1, p2|
        p1 ||= "0"
        p2 ||= "0"
        release1 = p1 =~ /^[0-9]+$/
        release2 = p2 =~ /^[0-9]+$/
        if release1 && release2
          # Integer comparison if both are full releases
          p1, p2 = p1.to_i, p2.to_i
          next if p1 == p2
          return p1 > p2
        elsif !release1 && !release2
          # String comparison if both are prereleases
          next if p1 == p2
          return p1 > p2
        else
          # If only one is a release, that one is newer
          return release1
        end
      end
    end

    # Returns whether one version string represents the same or a more
    # recent version than another.
    #
    # @param v1 [String] A version string.
    # @param v2 [String] Another version string.
    # @return [Boolean]
    def version_geq(v1, v2)
      version_gt(v1, v2) || !version_gt(v2, v1)
    end

    # Throws a NotImplementedError for an abstract method.
    #
    # @param obj [Object] `self`
    # @raise [NotImplementedError]
    def abstract(obj)
      raise NotImplementedError.new("#{obj.class} must implement ##{caller_info[2]}")
    end

    # Prints a deprecation warning for the caller method.
    #
    # @param obj [Object] `self`
    # @param message [String] A message describing what to do instead.
    def deprecated(obj, message = nil)
      obj_class = obj.is_a?(Class) ? "#{obj}." : "#{obj.class}#"
      full_message = "DEPRECATION WARNING: #{obj_class}#{caller_info[2]} " +
        "will be removed in a future version of Sass.#{("\n" + message) if message}"
      Sass::Util.sass_warn full_message
    end

    # Silence all output to STDERR within a block.
    #
    # @yield A block in which no output will be printed to STDERR
    def silence_warnings
      the_real_stderr, $stderr = $stderr, StringIO.new
      yield
    ensure
      $stderr = the_real_stderr
    end

    # Silences all Sass warnings within a block.
    #
    # @yield A block in which no Sass warnings will be printed
    def silence_sass_warnings
      old_level, Sass.logger.log_level = Sass.logger.log_level, :error
      yield
    ensure
      Sass.logger.log_level = old_level
    end

    # The same as `Kernel#warn`, but is silenced by \{#silence\_sass\_warnings}.
    #
    # @param msg [String]
    def sass_warn(msg)
      msg = msg + "\n" unless ruby1?
      Sass.logger.warn(msg)
    end

    ## Cross Rails Version Compatibility

    # Returns the root of the Rails application,
    # if this is running in a Rails context.
    # Returns `nil` if no such root is defined.
    #
    # @return [String, nil]
    def rails_root
      if defined?(::Rails.root)
        return ::Rails.root.to_s if ::Rails.root
        raise "ERROR: Rails.root is nil!"
      end
      return RAILS_ROOT.to_s if defined?(RAILS_ROOT)
      nil
    end

    # Returns the environment of the Rails application,
    # if this is running in a Rails context.
    # Returns `nil` if no such environment is defined.
    #
    # @return [String, nil]
    def rails_env
      return ::Rails.env.to_s if defined?(::Rails.env)
      return RAILS_ENV.to_s if defined?(RAILS_ENV)
      nil
    end

    # Returns whether this environment is using ActionPack
    # version 3.0.0 or greater.
    #
    # @return [Boolean]
    def ap_geq_3?
      ap_geq?("3.0.0.beta1")
    end

    # Returns whether this environment is using ActionPack
    # of a version greater than or equal to that specified.
    #
    # @param version [String] The string version number to check against.
    #   Should be greater than or equal to Rails 3,
    #   because otherwise ActionPack::VERSION isn't autoloaded
    # @return [Boolean]
    def ap_geq?(version)
      # The ActionPack module is always loaded automatically in Rails >= 3
      return false unless defined?(ActionPack) && defined?(ActionPack::VERSION) &&
        defined?(ActionPack::VERSION::STRING)

      version_geq(ActionPack::VERSION::STRING, version)
    end

    # Returns whether this environment is using Listen
    # version 2.0.0 or greater.
    #
    # @return [Boolean]
    def listen_geq_2?
      return @listen_geq_2 unless @listen_geq_2.nil?
      @listen_geq_2 =
        begin
          require 'listen/version'
          version_geq(::Listen::VERSION, '2.0.0')
        rescue LoadError
          false
        end
    end

    # Returns an ActionView::Template* class.
    # In pre-3.0 versions of Rails, most of these classes
    # were of the form `ActionView::TemplateFoo`,
    # while afterwards they were of the form `ActionView;:Template::Foo`.
    #
    # @param name [#to_s] The name of the class to get.
    #   For example, `:Error` will return `ActionView::TemplateError`
    #   or `ActionView::Template::Error`.
    def av_template_class(name)
      return ActionView.const_get("Template#{name}") if ActionView.const_defined?("Template#{name}")
      ActionView::Template.const_get(name.to_s)
    end

    ## Cross-OS Compatibility
    #
    # These methods are cached because some of them are called quite frequently
    # and even basic checks like String#== are too costly to be called repeatedly.

    # Whether or not this is running on Windows.
    #
    # @return [Boolean]
    def windows?
      return @windows if defined?(@windows)
      @windows = (RbConfig::CONFIG['host_os'] =~ /mswin|windows|mingw/i)
    end

    # Whether or not this is running on IronRuby.
    #
    # @return [Boolean]
    def ironruby?
      return @ironruby if defined?(@ironruby)
      @ironruby = RUBY_ENGINE == "ironruby"
    end

    # Whether or not this is running on Rubinius.
    #
    # @return [Boolean]
    def rbx?
      return @rbx if defined?(@rbx)
      @rbx = RUBY_ENGINE == "rbx"
    end

    # Whether or not this is running on JRuby.
    #
    # @return [Boolean]
    def jruby?
      return @jruby if defined?(@jruby)
      @jruby = RUBY_PLATFORM =~ /java/
    end

    # Returns an array of ints representing the JRuby version number.
    #
    # @return [Array<Fixnum>]
    def jruby_version
      @jruby_version ||= ::JRUBY_VERSION.split(".").map {|s| s.to_i}
    end

    # Like `Dir.glob`, but works with backslash-separated paths on Windows.
    #
    # @param path [String]
    def glob(path)
      path = path.gsub('\\', '/') if windows?
      if block_given?
        Dir.glob(path) {|f| yield(f)}
      else
        Dir.glob(path)
      end
    end

    # Like `Pathname.new`, but normalizes Windows paths to always use backslash
    # separators.
    #
    # `Pathname#relative_path_from` can break if the two pathnames aren't
    # consistent in their slash style.
    #
    # @param path [String]
    # @return [Pathname]
    def pathname(path)
      path = path.tr("/", "\\") if windows?
      Pathname.new(path)
    end

    # Like `Pathname#cleanpath`, but normalizes Windows paths to always use
    # backslash separators. Normally, `Pathname#cleanpath` actually does the
    # reverse -- it will convert backslashes to forward slashes, which can break
    # `Pathname#relative_path_from`.
    #
    # @param path [String, Pathname]
    # @return [Pathname]
    def cleanpath(path)
      path = Pathname.new(path) unless path.is_a?(Pathname)
      pathname(path.cleanpath.to_s)
    end

    # Prepare a value for a destructuring assignment (e.g. `a, b =
    # val`). This works around a performance bug when using
    # ActiveSupport, and only needs to be called when `val` is likely
    # to be `nil` reasonably often.
    #
    # See [this bug report](http://redmine.ruby-lang.org/issues/4917).
    #
    # @param val [Object]
    # @return [Object]
    def destructure(val)
      val || []
    end

    ## Cross-Ruby-Version Compatibility

    # Whether or not this is running under a Ruby version under 2.0.
    #
    # @return [Boolean]
    def ruby1?
      return @ruby1 if defined?(@ruby1)
      @ruby1 = Sass::Util::RUBY_VERSION[0] <= 1
    end

    # Whether or not this is running under Ruby 1.8 or lower.
    #
    # Note that IronRuby counts as Ruby 1.8,
    # because it doesn't support the Ruby 1.9 encoding API.
    #
    # @return [Boolean]
    def ruby1_8?
      # IronRuby says its version is 1.9, but doesn't support any of the encoding APIs.
      # We have to fall back to 1.8 behavior.
      return @ruby1_8 if defined?(@ruby1_8)
      @ruby1_8 = ironruby? ||
                   (Sass::Util::RUBY_VERSION[0] == 1 && Sass::Util::RUBY_VERSION[1] < 9)
    end

    # Whether or not this is running under Ruby 1.8.6 or lower.
    # Note that lower versions are not officially supported.
    #
    # @return [Boolean]
    def ruby1_8_6?
      return @ruby1_8_6 if defined?(@ruby1_8_6)
      @ruby1_8_6 = ruby1_8? && Sass::Util::RUBY_VERSION[2] < 7
    end

    # Wehter or not this is running under JRuby 1.6 or lower.
    def jruby1_6?
      return @jruby1_6 if defined?(@jruby1_6)
      @jruby1_6 = jruby? && jruby_version[0] == 1 && jruby_version[1] < 7
    end

    # Whether or not this is running under MacRuby.
    #
    # @return [Boolean]
    def macruby?
      return @macruby if defined?(@macruby)
      @macruby = RUBY_ENGINE == 'macruby'
    end

    require 'sass/util/ordered_hash' if ruby1_8?

    # Converts a hash or a list of pairs into an order-preserving hash.
    #
    # On Ruby 1.8.7, this uses the orderedhash gem to simulate an
    # order-preserving hash. On Ruby 1.9 and up, it just uses the native Hash
    # class, since that preserves the order itself.
    #
    # @overload ordered_hash(hash)
    #   @param hash [Hash] a normal hash to convert to an ordered hash
    #   @return [Hash]
    # @overload ordered_hash(*pairs)
    #   @example
    #     ordered_hash([:foo, "bar"], [:baz, "bang"])
    #       #=> {:foo => "bar", :baz => "bang"}
    #     ordered_hash #=> {}
    #   @param pairs [Array<(Object, Object)>] the list of key/value pairs for
    #     the hash.
    #   @return [Hash]
    def ordered_hash(*pairs_or_hash)
      if pairs_or_hash.length == 1 && pairs_or_hash.first.is_a?(Hash)
        hash = pairs_or_hash.first
        return hash unless ruby1_8?
        return OrderedHash.new.merge hash
      end

      return Hash[pairs_or_hash] unless ruby1_8?
      (pairs_or_hash.is_a?(NormalizedMap) ? NormalizedMap : OrderedHash)[*flatten(pairs_or_hash, 1)]
    end

    # Checks that the encoding of a string is valid in Ruby 1.9
    # and cleans up potential encoding gotchas like the UTF-8 BOM.
    # If it's not, yields an error string describing the invalid character
    # and the line on which it occurrs.
    #
    # @param str [String] The string of which to check the encoding
    # @yield [msg] A block in which an encoding error can be raised.
    #   Only yields if there is an encoding error
    # @yieldparam msg [String] The error message to be raised
    # @return [String] `str`, potentially with encoding gotchas like BOMs removed
    def check_encoding(str)
      if ruby1_8?
        return str.gsub(/\A\xEF\xBB\xBF/, '') # Get rid of the UTF-8 BOM
      elsif str.valid_encoding?
        # Get rid of the Unicode BOM if possible
        if str.encoding.name =~ /^UTF-(8|16|32)(BE|LE)?$/
          return str.gsub(Regexp.new("\\A\uFEFF".encode(str.encoding.name)), '')
        else
          return str
        end
      end

      encoding = str.encoding
      newlines = Regexp.new("\r\n|\r|\n".encode(encoding).force_encoding("binary"))
      str.force_encoding("binary").split(newlines).each_with_index do |line, i|
        begin
          line.encode(encoding)
        rescue Encoding::UndefinedConversionError => e
          yield <<MSG.rstrip, i + 1
Invalid #{encoding.name} character #{undefined_conversion_error_char(e)}
MSG
        end
      end
      str
    end

    # Like {\#check\_encoding}, but also checks for a `@charset` declaration
    # at the beginning of the file and uses that encoding if it exists.
    #
    # The Sass encoding rules are simple.
    # If a `@charset` declaration exists,
    # we assume that that's the original encoding of the document.
    # Otherwise, we use whatever encoding Ruby has.
    # Then we convert that to UTF-8 to process internally.
    # The UTF-8 end result is what's returned by this method.
    #
    # @param str [String] The string of which to check the encoding
    # @yield [msg] A block in which an encoding error can be raised.
    #   Only yields if there is an encoding error
    # @yieldparam msg [String] The error message to be raised
    # @return [(String, Encoding)] The original string encoded as UTF-8,
    #   and the source encoding of the string (or `nil` under Ruby 1.8)
    # @raise [Encoding::UndefinedConversionError] if the source encoding
    #   cannot be converted to UTF-8
    # @raise [ArgumentError] if the document uses an unknown encoding with `@charset`
    def check_sass_encoding(str, &block)
      return check_encoding(str, &block), nil if ruby1_8?
      # We allow any printable ASCII characters but double quotes in the charset decl
      bin = str.dup.force_encoding("BINARY")
      encoding = Sass::Util::ENCODINGS_TO_CHECK.find do |enc|
        re = Sass::Util::CHARSET_REGEXPS[enc]
        re && bin =~ re
      end
      charset, bom = $1, $2
      if charset
        charset = charset.force_encoding(encoding).encode("UTF-8")
        if (endianness = encoding[/[BL]E$/])
          begin
            Encoding.find(charset + endianness)
            charset << endianness
          rescue ArgumentError # Encoding charset + endianness doesn't exist
          end
        end
        str.force_encoding(charset)
      elsif bom
        str.force_encoding(encoding)
      end

      str = check_encoding(str, &block)
      return str.encode("UTF-8"), str.encoding
    end

    unless ruby1_8?
      # @private
      def _enc(string, encoding)
        string.encode(encoding).force_encoding("BINARY")
      end

      # We could automatically add in any non-ASCII-compatible encodings here,
      # but there's not really a good way to do that
      # without manually checking that each encoding
      # encodes all ASCII characters properly,
      # which takes long enough to affect the startup time of the CLI.
      ENCODINGS_TO_CHECK = %w[UTF-8 UTF-16BE UTF-16LE UTF-32BE UTF-32LE]

      CHARSET_REGEXPS = Hash.new do |h, e|
        h[e] =
          begin
            # /\A(?:\uFEFF)?@charset "(.*?)"|\A(\uFEFF)/
            Regexp.new(/\A(?:#{_enc("\uFEFF", e)})?#{
              _enc('@charset "', e)}(.*?)#{_enc('"', e)}|\A(#{
              _enc("\uFEFF", e)})/)
          rescue Encoding::ConverterNotFoundError => _
            nil # JRuby on Java 5 doesn't support UTF-32
          rescue
            # /\A@charset "(.*?)"/
            Regexp.new(/\A#{_enc('@charset "', e)}(.*?)#{_enc('"', e)}/)
          end
      end
    end

    # Checks to see if a class has a given method.
    # For example:
    #
    #     Sass::Util.has?(:public_instance_method, String, :gsub) #=> true
    #
    # Method collections like `Class#instance_methods`
    # return strings in Ruby 1.8 and symbols in Ruby 1.9 and on,
    # so this handles checking for them in a compatible way.
    #
    # @param attr [#to_s] The (singular) name of the method-collection method
    #   (e.g. `:instance_methods`, `:private_methods`)
    # @param klass [Module] The class to check the methods of which to check
    # @param method [String, Symbol] The name of the method do check for
    # @return [Boolean] Whether or not the given collection has the given method
    def has?(attr, klass, method)
      klass.send("#{attr}s").include?(ruby1_8? ? method.to_s : method.to_sym)
    end

    # A version of `Enumerable#enum_with_index` that works in Ruby 1.8 and 1.9.
    #
    # @param enum [Enumerable] The enumerable to get the enumerator for
    # @return [Enumerator] The with-index enumerator
    def enum_with_index(enum)
      ruby1_8? ? enum.enum_with_index : enum.each_with_index
    end

    # A version of `Enumerable#enum_cons` that works in Ruby 1.8 and 1.9.
    #
    # @param enum [Enumerable] The enumerable to get the enumerator for
    # @param n [Fixnum] The size of each cons
    # @return [Enumerator] The consed enumerator
    def enum_cons(enum, n)
      ruby1_8? ? enum.enum_cons(n) : enum.each_cons(n)
    end

    # A version of `Enumerable#enum_slice` that works in Ruby 1.8 and 1.9.
    #
    # @param enum [Enumerable] The enumerable to get the enumerator for
    # @param n [Fixnum] The size of each slice
    # @return [Enumerator] The consed enumerator
    def enum_slice(enum, n)
      ruby1_8? ? enum.enum_slice(n) : enum.each_slice(n)
    end

    # Destructively removes all elements from an array that match a block, and
    # returns the removed elements.
    #
    # @param array [Array] The array from which to remove elements.
    # @yield [el] Called for each element.
    # @yieldparam el [*] The element to test.
    # @yieldreturn [Boolean] Whether or not to extract the element.
    # @return [Array] The extracted elements.
    def extract!(array)
      out = []
      array.reject! do |e|
        next false unless yield e
        out << e
        true
      end
      out
    end

    # Returns the ASCII code of the given character.
    #
    # @param c [String] All characters but the first are ignored.
    # @return [Fixnum] The ASCII code of `c`.
    def ord(c)
      ruby1_8? ? c[0] : c.ord
    end

    # Flattens the first `n` nested arrays in a cross-version manner.
    #
    # @param arr [Array] The array to flatten
    # @param n [Fixnum] The number of levels to flatten
    # @return [Array] The flattened array
    def flatten(arr, n)
      return arr.flatten(n) unless ruby1_8_6?
      return arr if n == 0
      arr.inject([]) {|res, e| e.is_a?(Array) ? res.concat(flatten(e, n - 1)) : res << e}
    end

    # Flattens the first level of nested arrays in `arrs`. Unlike
    # `Array#flatten`, this orders the result by taking the first
    # values from each array in order, then the second, and so on.
    #
    # @param arrs [Array] The array to flatten.
    # @return [Array] The flattened array.
    def flatten_vertically(arrs)
      result = []
      arrs = arrs.map {|sub| sub.is_a?(Array) ? sub.dup : Array(sub)}
      until arrs.empty?
        arrs.reject! do |arr|
          result << arr.shift
          arr.empty?
        end
      end
      result
    end

    # Returns the hash code for a set in a cross-version manner.
    # Aggravatingly, this is order-dependent in Ruby 1.8.6.
    #
    # @param set [Set]
    # @return [Fixnum] The order-independent hashcode of `set`
    def set_hash(set)
      return set.hash unless ruby1_8_6?
      set.map {|e| e.hash}.uniq.sort.hash
    end

    # Tests the hash-equality of two sets in a cross-version manner.
    # Aggravatingly, this is order-dependent in Ruby 1.8.6.
    #
    # @param set1 [Set]
    # @param set2 [Set]
    # @return [Boolean] Whether or not the sets are hashcode equal
    def set_eql?(set1, set2)
      return set1.eql?(set2) unless ruby1_8_6?
      set1.to_a.uniq.sort_by {|e| e.hash}.eql?(set2.to_a.uniq.sort_by {|e| e.hash})
    end

    # Like `Object#inspect`, but preserves non-ASCII characters rather than
    # escaping them under Ruby 1.9.2.  This is necessary so that the
    # precompiled Haml template can be `#encode`d into `@options[:encoding]`
    # before being evaluated.
    #
    # @param obj {Object}
    # @return {String}
    def inspect_obj(obj)
      return obj.inspect unless version_geq(::RUBY_VERSION, "1.9.2")
      return ':' + inspect_obj(obj.to_s) if obj.is_a?(Symbol)
      return obj.inspect unless obj.is_a?(String)
      '"' + obj.gsub(/[\x00-\x7F]+/) {|s| s.inspect[1...-1]} + '"'
    end

    # Extracts the non-string vlaues from an array containing both strings and non-strings.
    # These values are replaced with escape sequences.
    # This can be undone using \{#inject\_values}.
    #
    # This is useful e.g. when we want to do string manipulation
    # on an interpolated string.
    #
    # The precise format of the resulting string is not guaranteed.
    # However, it is guaranteed that newlines and whitespace won't be affected.
    #
    # @param arr [Array] The array from which values are extracted.
    # @return [(String, Array)] The resulting string, and an array of extracted values.
    def extract_values(arr)
      values = []
      mapped = arr.map do |e|
        next e.gsub('{', '{{') if e.is_a?(String)
        values << e
        next "{#{values.count - 1}}"
      end
      return mapped.join, values
    end

    # Undoes \{#extract\_values} by transforming a string with escape sequences
    # into an array of strings and non-string values.
    #
    # @param str [String] The string with escape sequences.
    # @param values [Array] The array of values to inject.
    # @return [Array] The array of strings and values.
    def inject_values(str, values)
      return [str.gsub('{{', '{')] if values.empty?
      # Add an extra { so that we process the tail end of the string
      result = (str + '{{').scan(/(.*?)(?:(\{\{)|\{(\d+)\})/m).map do |(pre, esc, n)|
        [pre, esc ? '{' : '', n ? values[n.to_i] : '']
      end.flatten(1)
      result[-2] = '' # Get rid of the extra {
      merge_adjacent_strings(result).reject {|s| s == ''}
    end

    # Allows modifications to be performed on the string form
    # of an array containing both strings and non-strings.
    #
    # @param arr [Array] The array from which values are extracted.
    # @yield [str] A block in which string manipulation can be done to the array.
    # @yieldparam str [String] The string form of `arr`.
    # @yieldreturn [String] The modified string.
    # @return [Array] The modified, interpolated array.
    def with_extracted_values(arr)
      str, vals = extract_values(arr)
      str = yield str
      inject_values(str, vals)
    end

    # Builds a sourcemap file name given the generated CSS file name.
    #
    # @param css [String] The generated CSS file name.
    # @return [String] The source map file name.
    def sourcemap_name(css)
      css + ".map"
    end

    # Escapes certain characters so that the result can be used
    # as the JSON string value. Returns the original string if
    # no escaping is necessary.
    #
    # @param s [String] The string to be escaped
    # @return [String] The escaped string
    def json_escape_string(s)
      return s if s !~ /["\\\b\f\n\r\t]/

      result = ""
      s.split("").each do |c|
        case c
        when '"', "\\"
          result << "\\" << c
        when "\n" then result << "\\n"
        when "\t" then result << "\\t"
        when "\r" then result << "\\r"
        when "\f" then result << "\\f"
        when "\b" then result << "\\b"
        else
          result << c
        end
      end
      result
    end

    # Converts the argument into a valid JSON value.
    #
    # @param v [Fixnum, String, Array, Boolean, nil]
    # @return [String]
    def json_value_of(v)
      case v
      when Fixnum
        v.to_s
      when String
        "\"" + json_escape_string(v) + "\""
      when Array
        "[" + v.map {|x| json_value_of(x)}.join(",") + "]"
      when NilClass
        "null"
      when TrueClass
        "true"
      when FalseClass
        "false"
      else
        raise ArgumentError.new("Unknown type: #{v.class.name}")
      end
    end

    VLQ_BASE_SHIFT = 5
    VLQ_BASE = 1 << VLQ_BASE_SHIFT
    VLQ_BASE_MASK = VLQ_BASE - 1
    VLQ_CONTINUATION_BIT = VLQ_BASE

    BASE64_DIGITS = ('A'..'Z').to_a  + ('a'..'z').to_a + ('0'..'9').to_a  + ['+', '/']
    BASE64_DIGIT_MAP = begin
      map = {}
      Sass::Util.enum_with_index(BASE64_DIGITS).map do |digit, i|
        map[digit] = i
      end
      map
    end

    # Encodes `value` as VLQ (http://en.wikipedia.org/wiki/VLQ).
    #
    # @param value [Fixnum]
    # @return [String] The encoded value
    def encode_vlq(value)
      if value < 0
        value = ((-value) << 1) | 1
      else
        value <<= 1
      end

      result = ''
      begin
        digit = value & VLQ_BASE_MASK
        value >>= VLQ_BASE_SHIFT
        if value > 0
          digit |= VLQ_CONTINUATION_BIT
        end
        result << BASE64_DIGITS[digit]
      end while value > 0
      result
    end

    # This is a hack around the fact that you can't instantiate a URI parser on
    # 1.8, so we have to have this hacky stuff to work around it. When 1.8
    # support is dropped, we can remove this method.
    #
    # @private
    URI_ESCAPE = URI.const_defined?("DEFAULT_PARSER") ? URI::DEFAULT_PARSER : URI

    # URI-escape `string`.
    #
    # @param string [String]
    # @return [String]
    def escape_uri(string)
      URI_ESCAPE.escape string
    end

    # A cross-platform implementation of `File.absolute_path`.
    #
    # @param path [String]
    # @param dir_string [String] The directory to consider [path] relative to.
    # @return [String] The absolute version of `path`.
    def absolute_path(path, dir_string = nil)
      # Ruby 1.8 doesn't support File.absolute_path.
      return File.absolute_path(path, dir_string) unless ruby1_8?

      # File.expand_path expands "~", which we don't want.
      return File.expand_path(path, dir_string) unless path[0] == ?~
      File.expand_path(File.join(".", path), dir_string)
    end

    ## Static Method Stuff

    # The context in which the ERB for \{#def\_static\_method} will be run.
    class StaticConditionalContext
      # @param set [#include?] The set of variables that are defined for this context.
      def initialize(set)
        @set = set
      end

      # Checks whether or not a variable is defined for this context.
      #
      # @param name [Symbol] The name of the variable
      # @return [Boolean]
      def method_missing(name, *args)
        super unless args.empty? && !block_given?
        @set.include?(name)
      end
    end

    # @private
    ATOMIC_WRITE_MUTEX = Mutex.new

    # This creates a temp file and yields it for writing. When the
    # write is complete, the file is moved into the desired location.
    # The atomicity of this operation is provided by the filesystem's
    # rename operation.
    #
    # @param filename [String] The file to write to.
    # @param perms [Integer] The permissions used for creating this file.
    #   Will be masked by the process umask. Defaults to readable/writeable
    #   by all users however the umask usually changes this to only be writable
    #   by the process's user.
    # @yieldparam tmpfile [Tempfile] The temp file that can be written to.
    # @return The value returned by the block.
    def atomic_create_and_write_file(filename, perms = 0666)
      require 'tempfile'
      tmpfile = Tempfile.new(File.basename(filename), File.dirname(filename))
      tmpfile.binmode if tmpfile.respond_to?(:binmode)
      result = yield tmpfile
      tmpfile.close
      ATOMIC_WRITE_MUTEX.synchronize do
        begin
          File.chmod(perms & ~File.umask, tmpfile.path)
        rescue Errno::EPERM
          # If we don't have permissions to chmod the file, don't let that crash
          # the compilation. See issue 1215.
        end
        File.rename tmpfile.path, filename
      end
      result
    ensure
      # close and remove the tempfile if it still exists,
      # presumably due to an error during write
      tmpfile.close if tmpfile
      tmpfile.unlink if tmpfile
    end

    def load_listen!
      if defined?(gem)
        begin
          gem 'listen', '>= 1.1.0', '< 3.0.0'
          require 'listen'
        rescue Gem::LoadError
          dir = scope("vendor/listen/lib")
          $LOAD_PATH.unshift dir
          begin
            require 'listen'
          rescue LoadError => e
            if version_geq(RUBY_VERSION, "1.9.3")
              version_constraint = "~> 2.7"
            else
              version_constraint = "~> 1.1"
            end
            e.message << "\n" <<
              "Run \"gem install listen --version '#{version_constraint}'\" to get it."
            raise e
          end
        end
      else
        begin
          require 'listen'
        rescue LoadError => e
          dir = scope("vendor/listen/lib")
          if $LOAD_PATH.include?(dir)
            raise e unless File.exist?(scope(".git"))
            e.message << "\n" <<
              'Run "git submodule update --init" to get the bundled version.'
          else
            $LOAD_PATH.unshift dir
            retry
          end
        end
      end
    end

    private

    # rubocop:disable LineLength

    # Calculates the memoization table for the Least Common Subsequence algorithm.
    # Algorithm from [Wikipedia](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem#Computing_the_length_of_the_LCS)
    def lcs_table(x, y)
      # This method does not take a block as an explicit parameter for performance reasons.
      # rubocop:enable LineLength
      c = Array.new(x.size) {[]}
      x.size.times {|i| c[i][0] = 0}
      y.size.times {|j| c[0][j] = 0}
      (1...x.size).each do |i|
        (1...y.size).each do |j|
          c[i][j] =
            if yield x[i], y[j]
              c[i - 1][j - 1] + 1
            else
              [c[i][j - 1], c[i - 1][j]].max
            end
        end
      end
      c
    end
    # rubocop:disable ParameterLists, LineLength

    # Computes a single longest common subsequence for arrays x and y.
    # Algorithm from [Wikipedia](http://en.wikipedia.org/wiki/Longest_common_subsequence_problem#Reading_out_an_LCS)
    def lcs_backtrace(c, x, y, i, j, &block)
      # rubocop:enable ParameterList, LineLengths
      return [] if i == 0 || j == 0
      if (v = yield(x[i], y[j]))
        return lcs_backtrace(c, x, y, i - 1, j - 1, &block) << v
      end

      return lcs_backtrace(c, x, y, i, j - 1, &block) if c[i][j - 1] > c[i - 1][j]
      lcs_backtrace(c, x, y, i - 1, j, &block)
    end

    singleton_methods.each {|method| module_function method}
  end
end

require 'sass/util/multibyte_string_scanner'
require 'sass/util/normalized_map'
require 'sass/util/cross_platform_random'
