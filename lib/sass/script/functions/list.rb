require "sass/script/functions"

module Sass::Script::Functions

  # Creates a space-delimited list from the arguments.
  #
  # @example
  #   list(10px, solid, blue) => 10px solid blue
  #
  # @param values [Array<Literal>] The values for the list
  # @return [SpaceList] the list literal
  def list(*values)
    Sass::Script::SpaceList.new(values)
  end
  define :list, :var_args => true

  # Creates a comma-delimited list from the arguments.
  #
  # @example
  #   comma-list(10px solid blue) => 10px, solid, blue
  #
  # @param values [Array<Literal>] The values for the list
  # @return [CommaList] the list literal
  def comma_list(*values)
    Sass::Script::CommaList.new(values)
  end
  define :comma_list, :var_args => true

  # Returns the value at the nth index of the list.
  # Special values of `first` and `last` can also be passed for the index.
  #
  # @example
  #   nth(10px solid blue, 1) => 10px
  #   nth(10px solid blue, first) => 10px
  #   nth(10px solid blue, 3) => blue
  #   nth(10px solid blue, last) => blue
  #
  # @return [SpaceList] the list literal
  def nth(list, index)
    assert_type list, :List
    idx = assert_index index, 1, list.elements.size
    list.elements[idx - 1]
  end
  define :nth, :args => [:list, :index]

  # Add one or more elements to the end of a list
  #
  # @example
  #   append(10px solid, blue) => 10px solid blue
  #   append(one two, three, four) => one two three four
  #   append((one, two), three, four) => one, two, three, four
  #   append($list: (one, two), $elements: (three, four)) => one, two, three, four
  #
  # @return [List] a new list
  def append(list, *elements)
    assert_type list, :List
    if elements.last.is_a? Hash
      elements = extract_named_arguments(elements, "elements").first
      assert_type elements, :List
      elements = elements.to_a
    end
    list.class.new(list.elements + elements)
  end
  define :append, :args => [:list], :var_args => true
  define :append, :args => [:list], :var_kwargs => true

  # Add one or more elements to the beginning of a list
  #
  # @example
  #   prepend(10px solid, blue) => blue 10px solid 
  #   prepend(one two, three, four) => three four one two
  #   prepend((one, two), three, four) => three, four, one, two
  #   prepend($list: (one, two), $elements: (three, four)) => three, four, one, two
  #
  # @return [List] a new list
  def prepend(list, *elements)
    assert_type list, :List
    if elements.last.is_a? Hash
      elements = extract_named_arguments(elements, "elements").first
      assert_type elements, :List
      elements = elements.to_a
    end
    list.class.new(elements + list.elements)
  end
  define :prepend, :args => [:list], :var_args => true
  define :prepend, :args => [:list], :var_kwargs => true

  # Combine several lists into a single list.
  #
  # @example
  #   concat(10px solid, red green) => 10px solid red green
  #   concat((10px, solid), red green) => 10px, solid, red, green
  #   concat(10px solid, (red, green)) => 10px solid red green
  #   concat(one two, three four, five six) => one two three four five six
  #   concat($list: one two, $lists: (three four, five six)) => one two three four five six
  #
  # @return [List] a new list with the same delimiter as the first list
  def concat(list, *lists)
    assert_type list, :List
    elements = list.elements.dup
    if lists.last.is_a? Hash
      lists = extract_named_arguments(lists, "lists").first
      assert_type lists, :List
      lists = lists.to_a
    end
    lists.each do |l|
      assert_type l, :List
      elements += l.elements
    end
    list.class.new(elements)
  end
  define :concat, :args => [:list], :var_args => true
  define :concat, :args => [:list], :var_kwargs => true

  # Extract a sublist from a list of the elements between `from` and `to` inclusive
  #
  # @example
  #   slice(10px solid red green, 2, 3) => solid red
  #   slice((10px, solid, red, green), 2, 3) => solid, red
  #   slice($list: (10px, solid, red, green), $from: 2, $to: 3) => solid, red
  #
  # @return [List] a new list
  def slice(list, from, to)
    assert_type list, :List
    from_int = assert_index from, 1, list.elements.size
    to_int = assert_index to, from_int, list.elements.size
    list.class.new(list.elements[(from_int-1)..(to_int-1)])
  end
  define :slice, :args => [:list, :from, :to]

  # Count how many elements are in a list.
  #
  # @example
  #   count(10px solid red green) => 4
  #   count(10px solid red green) => 4
  #
  # @return [Number] The size of the list
  def count(list)
    assert_type list, :List
    Sass::Script::Number.new(list.elements.size)
  end
  define :count, :args => [:list]

  # Check whether a list contains all of the specified values.
  #
  # @example
  #   contains(a b c d, a) => true
  #   contains(a b c d, b) => true
  #   contains(a b c d, z) => false
  #   contains(a b c d, a, c) => true
  #   contains(a b c d, a, z) => false
  #
  # @return [Bool] whether the list contains the values
  def contains(list, *values)
    assert_type list, :List
    values.each do |value|
      return Sass::Script::Bool.new(false) unless list.elements.include?(value)
    end
    Sass::Script::Bool.new(true)
  end
  define :contains, :args => [:list], :var_args => true

  # Combine several lists of equal counts into one list
  # where each element is a list of the values at that same
  # index in the original lists.
  #
  # Zip does not accept named arguments.
  #
  # @example
  #   zip(a b, c d) => a c, b d
  #   zip((a, b), (c, d), (e, f)) => a c e, b d f
  #
  # @return [List] a new list
  # @raise [Sass::SyntaxError] if the list have different sizes
  def zip(list1, *lists)
    assert_type list1, :List
    lists.each do |l|
      assert_type l, :List
      unless l.elements.size == list1.elements.size
        raise ArgumentError, "#{l.inspect} expected to have #{list1.elements.size} elements"
      end
    end
    Sass::Script::CommaList.new(list1.elements.zip(*lists.map{|l| l.elements}).map{|l| Sass::Script::SpaceList.new(l)})
  end

  # Create a new list where the elements are the results
  # of successively calling the passed function with the
  # list as the first argument.
  # 
  # Any additional arguments will be passed to the function
  # after the list element.
  #
  # @example
  #   map(url, "foo.png" "bar.gif") => url("foo.png") url("bar.gif")
  #   map(alpha, #000 rgba(#000, 0.5)) => 1 0.5
  #   map(comparable, 2px 2pc 2in 2em, 1cm) => false true true false
  #   map(comparable, 2px 2pc 2in 2em, $number-2: 1cm) => false true true false
  #
  # @return [List] A new list containing the return values
  def map(fn, list, *args)
    assert_type fn, :String
    assert_type list, :List
    applied = list.elements.map do |el|
      funcall = Sass::Script::Funcall.new(fn.value, [el] + args)
      funcall.options = fn.options
      funcall.instance_variable_set("@context", fn.context)
      funcall.perform(environment)
    end
    list.class.new(applied)
  end
  define :map, :args => [:function, :list], :var_args => true, :var_kwargs => true

  # Calls a function using a list as the arguments
  #
  # @example
  #   apply(rgb, 255 0 127) => #ff007f
  #   apply($function: rgb, $arguments: 255 0 127) => #ff007f
  #
  # @return [List] A new list containing the return values
  def apply(fn, arguments)
    assert_type fn, :String
    assert_type arguments, :List
    funcall = Sass::Script::Funcall.new(fn.value, arguments.elements)
    funcall.options = fn.options
    funcall.context = fn.context
    funcall.perform(environment)
  end
  define :apply, :args => [:function, :arguments]

  private

  def assert_index(index, min, max)
    if index.is_a?(Sass::Script::String)
      if "first" == index.value
        index = Sass::Script::Number.new(1)
      elsif "last" == index.value
        index = Sass::Script::Number.new(max)
      end
    end
    assert_type index, :Number
    unless index.unitless?
      raise Sass::SyntaxError, "units are not allowed for index. Got: #{index.to_s}"
    end
    unless index.int?
      raise Sass::SyntaxError, "integer expected for index. Got: #{index.to_s}"
    end
    index = index.to_i
    if index > max
      raise Sass::SyntaxError, "index of #{index} out of range of #{max}"
    elsif index < min
      raise Sass::SyntaxError, "index #{index} cannot be less than #{min}"
    end
    index
  end

  def extract_named_arguments(arglist, *args)
    raise ArgumentError, "No named arguments found" unless arglist.last.is_a?(Hash)
    named_args = arglist.last
    
    if named_args.size > args.size
      raise ArgumentError, "Too many named arguments"
    end
    for name in args
      unless named_args.has_key?(name)
        raise ArgumentError, "Expected argument $#{name} not provided"
      end
    end
    args.inject([]) {|extracted_args, name| extracted_args << named_args[name] }
  end

end
