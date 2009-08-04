require 'erb'
require 'set'
require 'enumerator'

module Haml
  # A module containing various useful functions.
  module Util
    extend self

    # An array of ints representing the Ruby version number.
    RUBY_VERSION = ::RUBY_VERSION.split(".").map {|s| s.to_i}

    # Returns the path of a file relative to the Haml root directory.
    #
    # @param file [String] The filename relative to the Haml root
    # @return [String] The filename relative to the the working directory
    def scope(file)
      File.join(File.dirname(File.dirname(File.dirname(__FILE__))), file)
    end

    # Converts an array of `[key, value]` pairs to a hash.
    # For example:
    #
    #     to_hash([[:foo, "bar"], [:baz, "bang"]])
    #       #=> {:foo => "bar", :baz => "bang"}
    #
    # @param arr [Array<(Object, Object)>] An array of pairs
    # @return [Hash] A hash
    def to_hash(arr)
      arr.compact.inject({}) {|h, (k, v)| h[k] = v; h}
    end

    # Maps the keys in a hash according to a block.
    # For example:
    #
    #     map_keys({:foo => "bar", :baz => "bang"}) {|k| k.to_s}
    #       #=> {"foo" => "bar", "baz" => "bang"}
    #
    # @param hash [Hash] The hash to map
    # @yield [key] A block in which the keys are transformed
    # @yieldparam key [Object] The key that should be mapped
    # @yieldreturn [Object] The new value for the key
    # @return [Hash] The mapped hash
    # @see #map_vals
    # @see #map_hash
    def map_keys(hash)
      to_hash(hash.map {|k, v| [yield(k), v]})
    end

    # Maps the values in a hash according to a block.
    # For example:
    #
    #     map_values({:foo => "bar", :baz => "bang"}) {|v| v.to_sym}
    #       #=> {:foo => :bar, :baz => :bang}
    #
    # @param hash [Hash] The hash to map
    # @yield [value] A block in which the values are transformed
    # @yieldparam value [Object] The value that should be mapped
    # @yieldreturn [Object] The new value for the value
    # @return [Hash] The mapped hash
    # @see #map_keys
    # @see #map_hash
    def map_vals(hash)
      to_hash(hash.map {|k, v| [k, yield(v)]})
    end

    # Maps the key-value pairs of a hash according to a block.
    # For example:
    #
    #     map_hash({:foo => "bar", :baz => "bang"}) {|k, v| [k.to_s, v.to_sym]}
    #       #=> {"foo" => :bar, "baz" => :bang}
    #
    # @param hash [Hash] The hash to map
    # @yield [key, value] A block in which the key-value pairs are transformed
    # @yieldparam [key] The hash key
    # @yieldparam [value] The hash value
    # @yieldreturn [(Object, Object)] The new value for the `[key, value]` pair
    # @return [Hash] The mapped hash
    # @see #map_keys
    # @see #map_vals
    def map_hash(hash, &block)
      to_hash(hash.map(&block))
    end

    # Computes the powerset of the given array.
    # This is the set of all subsets of the array.
    # For example:
    #
    #     powerset([1, 2, 3]) #=>
    #       Set[Set[], Set[1], Set[2], Set[3], Set[1, 2], Set[2, 3], Set[1, 3], Set[1, 2, 3]]
    #
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

    # Concatenates all strings that are adjacent in an array,
    # while leaving other elements as they are.
    # For example:
    #
    #     merge_adjacent_strings([1, "foo", "bar", 2, "baz"])
    #       #=> [1, "foobar", 2, "baz"]
    #
    # @param enum [Enumerable]
    # @return [Array] The enumerable with strings merged
    def merge_adjacent_strings(enum)
      e = enum.inject([]) do |a, e|
        if e.is_a?(String) && a.last.is_a?(String)
          a.last << e
        else
          a << e
        end
        a
      end
    end

    # Whether or not this is running under Ruby 1.8 or lower.
    #
    # @return [Boolean]
    def ruby1_8?
      Haml::Util::RUBY_VERSION[0] == 1 && Haml::Util::RUBY_VERSION[1] < 9
    end

    # Checks to see if a class has a given method.
    # For example:
    #
    #     Haml::Util.has?(:public_instance_method, String, :gsub) #=> true
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
      def method_missing(name, *args, &block)
        super unless args.empty? && block.nil?
        @set.include?(name)
      end
    end

    # This is used for methods in {Haml::Buffer} that need to be very fast,
    # and take a lot of boolean parameters
    # that are known at compile-time.
    # Instead of passing the parameters in normally,
    # a separate method is defined for every possible combination of those parameters;
    # these are then called using \{#static\_method\_name}.
    #
    # To define a static method, an ERB template for the method is provided.
    # All conditionals based on the static parameters
    # are done as embedded Ruby within this template.
    # For example:
    #
    #     def_static_method(Foo, :my_static_method, [:foo, :bar], :baz, :bang, <<RUBY)
    #       <% if baz && bang %>
    #         return foo + bar
    #       <% elsif baz || bang %>
    #         return foo - bar
    #       <% else %>
    #         return 17
    #       <% end %>
    #     RUBY
    #
    # \{#static\_method\_name} can be used to call static methods.
    #
    # @overload def_static_method(klass, name, args, *vars, erb)
    # @param klass [Module] The class on which to define the static method
    # @param name [#to_s] The (base) name of the static method
    # @param args [Array<Symbol>] The names of the arguments to the defined methods
    #   (**not** to the ERB template)
    # @param vars [Array<Symbol>] The names of the static boolean variables
    #   to be made available to the ERB template
    # @param erb [String] The template for the method code
    def def_static_method(klass, name, args, *vars)
      erb = vars.pop
      powerset(vars).each do |set|
        context = StaticConditionalContext.new(set).instance_eval {binding}
        klass.class_eval(<<METHOD)
def #{static_method_name(name, *vars.map {|v| set.include?(v)})}(#{args.join(', ')})
  #{ERB.new(erb).result(context)}
end
METHOD
      end
    end

    # Computes the name for a method defined via \{#def\_static\_method}.
    #
    # @param name [String] The base name of the static method
    # @param vars [Array<Boolean>] The static variable assignment
    # @return [String] The real name of the static method
    def static_method_name(name, *vars)
      "#{name}_#{vars.map {|v| !!v}.join('_')}"
    end
  end
end
