# A visitor for converting a dynamic Sass tree into a static Sass tree.
class Sass::Tree::Visitors::Perform < Sass::Tree::Visitors::Base
  # @param root [Tree::Node] The root node of the tree to visit.
  # @param environment [Sass::Environment] The lexical environment.
  # @return [Tree::Node] The resulting tree of static nodes.
  def self.visit(root, environment = Sass::Environment.new)
    new(environment).send(:visit, root)
  end

  protected

  def initialize(env)
    @environment = env
    # Stack trace information, including mixin includes and imports.
    @stack = []
  end

  # If an exception is raised, this adds proper metadata to the backtrace.
  def visit(node)
    super(node.dup)
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Keeps track of the current environment.
  def visit_children(parent)
    with_environment Sass::Environment.new(@environment, parent.options) do
      parent.children = super.flatten
      parent
    end
  end

  # Runs a block of code with the current environment replaced with the given one.
  #
  # @param env [Sass::Environment] The new environment for the duration of the block.
  # @yield A block in which the environment is set to `env`.
  # @return [Object] The return value of the block.
  def with_environment(env)
    old_env, @environment = @environment, env
    yield
  ensure
    @environment = old_env
  end

  # Sets the options on the environment if this is the top-level root.
  def visit_root(node)
    yield
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  # Removes this node from the tree if it's a silent comment.
  def visit_comment(node)
    return [] if node.invisible?
    node.resolved_value = run_interp_no_strip(node.value)
    node.resolved_value.gsub!(/\\([\\#])/, '\1')
    node
  end

  # Prints the expression to STDERR.
  def visit_debug(node)
    res = node.expr.perform(@environment)
    res = res.value if res.is_a?(Sass::Script::String)
    if node.filename
      $stderr.puts "#{node.filename}:#{node.line} DEBUG: #{res}"
    else
      $stderr.puts "Line #{node.line} DEBUG: #{res}"
    end
    []
  end

  # Runs the child nodes once for each value in the list.
  def visit_each(node)
    list = node.list.perform(@environment)

    with_environment Sass::Environment.new(@environment) do
      list.to_a.map do |v|
        @environment.set_local_var(node.var, v)
        node.children.map {|c| visit(c)}
      end.flatten
    end
  end

  # Runs SassScript interpolation in the selector,
  # and then parses the result into a {Sass::Selector::CommaSequence}.
  def visit_extend(node)
    parser = Sass::SCSS::StaticParser.new(run_interp(node.selector), node.filename, node.line)
    node.resolved_selector = parser.parse_selector
    node
  end

  # Runs the child nodes once for each time through the loop, varying the variable each time.
  def visit_for(node)
    from = node.from.perform(@environment)
    to = node.to.perform(@environment)
    from.assert_int!
    to.assert_int!

    to = to.coerce(from.numerator_units, from.denominator_units)
    range = Range.new(from.to_i, to.to_i, node.exclusive)

    with_environment Sass::Environment.new(@environment) do
      range.map do |i|
        @environment.set_local_var(node.var,
          Sass::Script::Number.new(i, from.numerator_units, from.denominator_units))
        node.children.map {|c| visit(c)}
      end.flatten
    end
  end

  # Loads the function into the environment.
  def visit_function(node)
    env = Sass::Environment.new(@environment, node.options)
    @environment.set_local_function(node.name,
      Sass::Callable.new(node.name, node.args, env, node.children, !:has_content))
    []
  end

  # Runs the child nodes if the conditional expression is true;
  # otherwise, tries the else nodes.
  def visit_if(node)
    if node.expr.nil? || node.expr.perform(@environment).to_bool
      yield
      node.children
    elsif node.else
      visit(node.else)
    else
      []
    end
  end

  # Returns a static DirectiveNode if this is importing a CSS file,
  # or parses and includes the imported Sass file.
  def visit_import(node)
    if path = node.css_import?
      return Sass::Tree::CssImportNode.resolved("url(#{path})")
    end
    file = node.imported_file
    handle_import_loop!(node) if @stack.any? {|e| e[:filename] == file.options[:filename]}

    @stack.push(:filename => node.filename, :line => node.line)
    root = file.to_tree
    Sass::Tree::Visitors::CheckNesting.visit(root)
    node.children = root.children.map {|c| visit(c)}.flatten
    node
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.imported_file.options[:filename])
    e.add_backtrace(:filename => node.filename, :line => node.line)
    raise e
  ensure
    @stack.pop unless path
  end

  # Loads a mixin into the environment.
  def visit_mixindef(node)
    env = Sass::Environment.new(@environment, node.options)
    @environment.set_local_mixin(node.name,
      Sass::Callable.new(node.name, node.args, env, node.children, node.has_content))
    []
  end

  # Runs a mixin.
  def visit_mixin(node)
    include_loop = true
    handle_include_loop!(node) if @stack.any? {|e| e[:name] == node.name}
    include_loop = false

    @stack.push(:filename => node.filename, :line => node.line, :name => node.name)
    raise Sass::SyntaxError.new("Undefined mixin '#{node.name}'.") unless mixin = @environment.mixin(node.name)

    if node.children.any? && !mixin.has_content
      raise Sass::SyntaxError.new(%Q{Mixin "#{node.name}" does not accept a content block.})
    end

    passed_args = node.args.dup
    passed_keywords = node.keywords.dup

    raise Sass::SyntaxError.new(<<END.gsub("\n", "")) if mixin.args.size < passed_args.size
Mixin #{node.name} takes #{mixin.args.size} argument#{'s' if mixin.args.size != 1}
 but #{node.args.size} #{node.args.size == 1 ? 'was' : 'were'} passed.
END

    passed_keywords.each do |name, value|
      # TODO: Make this fast
      unless mixin.args.find {|(var, default)| var.underscored_name == name}
        raise Sass::SyntaxError.new("Mixin #{node.name} doesn't have an argument named $#{name}")
      end
    end

    environment = mixin.args.zip(passed_args).
      inject(Sass::Environment.new(mixin.environment)) do |env, ((var, default), value)|
      env.set_local_var(var.name,
        if value
          value.perform(@environment)
        elsif kv = passed_keywords[var.underscored_name]
          kv.perform(@environment)
        elsif default
          default.perform(env)
        end)
      raise Sass::SyntaxError.new("Mixin #{node.name} is missing argument #{var.inspect}.") unless env.var(var.name)
      env
    end
    environment.caller = Sass::Environment.new(@environment)
    environment.content = node.children if node.has_children

    trace_node = Sass::Tree::TraceNode.from_node(node.name, node)
    with_environment(environment) {trace_node.children = mixin.tree.map {|c| visit(c)}.flatten}
    trace_node
  rescue Sass::SyntaxError => e
    unless include_loop
      e.modify_backtrace(:mixin => node.name, :line => node.line)
      e.add_backtrace(:line => node.line)
    end
    raise e
  ensure
    @stack.pop unless include_loop
  end

  def visit_content(node)
    raise Sass::SyntaxError.new("No @content passed.") unless content = @environment.content
    @stack.push(:filename => node.filename, :line => node.line, :name => '@content')
    trace_node = Sass::Tree::TraceNode.from_node('@content', node)
    with_environment(@environment.caller) {trace_node.children = content.map {|c| visit(c.dup)}.flatten}
    trace_node
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:mixin => '@content', :line => node.line)
    e.add_backtrace(:line => node.line)
    raise e
  ensure
    @stack.pop if content
  end

  # Runs any SassScript that may be embedded in a property.
  def visit_prop(node)
    node.resolved_name = run_interp(node.name)
    val = node.value.perform(@environment)
    node.resolved_value = val.to_s
    yield
  end

  # Returns the value of the expression.
  def visit_return(node)
    throw :_sass_return, node.expr.perform(@environment)
  end

  # Runs SassScript interpolation in the selector,
  # and then parses the result into a {Sass::Selector::CommaSequence}.
  def visit_rule(node)
    rule = node.rule
    rule = rule.map {|e| e.is_a?(String) && e != ' ' ? e.strip : e} if node.style == :compressed
    parser = Sass::SCSS::StaticParser.new(run_interp(node.rule), node.filename, node.line)
    node.parsed_rules ||= parser.parse_selector
    if node.options[:trace_selectors]
      @stack.push(:filename => node.filename, :line => node.line)
      node.stack_trace = stack_trace
      @stack.pop
    end
    yield
  end

  # Loads the new variable value into the environment.
  def visit_variable(node)
    var = @environment.var(node.name)
    return [] if node.guarded && var && !var.null?
    val = node.expr.perform(@environment)
    @environment.set_var(node.name, val)
    []
  end

  # Prints the expression to STDERR with a stylesheet trace.
  def visit_warn(node)
    @stack.push(:filename => node.filename, :line => node.line)
    res = node.expr.perform(@environment)
    res = res.value if res.is_a?(Sass::Script::String)
    msg = "WARNING: #{res}\n         "
    msg << stack_trace.join("\n         ")
    # JRuby doesn't automatically add a newline for #warn
    msg << (RUBY_PLATFORM =~ /java/ ? "\n\n" : "\n")
    Sass::Util.sass_warn msg
    []
  ensure
    @stack.pop
  end

  # Runs the child nodes until the continuation expression becomes false.
  def visit_while(node)
    children = []
    with_environment Sass::Environment.new(@environment) do
      children += node.children.map {|c| visit(c)} while node.expr.perform(@environment).to_bool
    end
    children.flatten
  end

  def visit_directive(node)
    node.resolved_value = run_interp(node.value)
    yield
  end

  def visit_media(node)
    parser = Sass::SCSS::StaticParser.new(run_interp(node.query), node.filename, node.line)
    node.resolved_query ||= parser.parse_media_query_list
    yield
  end

  def visit_supports(node)
    node.condition = node.condition.deep_copy
    node.condition.perform(@environment)
    yield
  end

  def visit_cssimport(node)
    node.resolved_uri = run_interp([node.uri])
    if node.query
      parser = Sass::SCSS::StaticParser.new(run_interp(node.query), node.filename, node.line)
      node.resolved_query ||= parser.parse_media_query_list
    end
    yield
  end

  private

  def stack_trace
    trace = []
    stack = @stack.map {|e| e.dup}.reverse
    stack.each_cons(2) {|(e1, e2)| e1[:caller] = e2[:name]; [e1, e2]}
    stack.each_with_index do |entry, i|
      msg = "#{i == 0 ? "on" : "from"} line #{entry[:line]}"
      msg << " of #{entry[:filename] || "an unknown file"}"
      msg << ", in `#{entry[:caller]}'" if entry[:caller]
      trace << msg
    end
    trace
  end

  def run_interp_no_strip(text)
    text.map do |r|
      next r if r.is_a?(String)
      val = r.perform(@environment)
      # Interpolated strings should never render with quotes
      next val.value if val.is_a?(Sass::Script::String)
      val.to_s
    end.join
  end

  def run_interp(text)
    run_interp_no_strip(text).strip
  end

  def handle_include_loop!(node)
    msg = "An @include loop has been found:"
    content_count = 0
    mixins = @stack.reverse.map {|s| s[:name]}.compact.select do |s|
      if s == '@content'
        content_count += 1
        false
      elsif content_count > 0
        content_count -= 1
        false
      else
        true
      end
    end

    return if mixins.empty?
    raise Sass::SyntaxError.new("#{msg} #{node.name} includes itself") if mixins.size == 1

    msg << "\n" << Sass::Util.enum_cons(mixins.reverse + [node.name], 2).map do |m1, m2|
      "    #{m1} includes #{m2}"
    end.join("\n")
    raise Sass::SyntaxError.new(msg)
  end

  def handle_import_loop!(node)
    msg = "An @import loop has been found:"
    files = @stack.map {|s| s[:filename]}.compact
    if node.filename == node.imported_file.options[:filename]
      raise Sass::SyntaxError.new("#{msg} #{node.filename} imports itself")
    end

    files << node.filename << node.imported_file.options[:filename]
    msg << "\n" << Sass::Util.enum_cons(files, 2).map do |m1, m2|
      "    #{m1} imports #{m2}"
    end.join("\n")
    raise Sass::SyntaxError.new(msg)
  end
end
