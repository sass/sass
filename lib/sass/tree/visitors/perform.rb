# A visitor for converting a dynamic Sass tree into a static Sass tree.
class Sass::Tree::Visitors::Perform < Sass::Tree::Visitors::Base
  # @param root [Tree::Node] The root node of the tree to visit.
  # @param environment [Sass::Environment] The lexical environment.
  # @return [Tree::Node] The resulting tree of static nodes.
  def self.visit(root, environment = nil)
    new(environment).send(:visit, root)
  end

  # @api private
  def self.perform_arguments(callable, args, keywords, splat)
    desc = "#{callable.type.capitalize} #{callable.name}"
    downcase_desc = "#{callable.type} #{callable.name}"

    begin
      unless keywords.empty?
        unknown_args = Sass::Util.array_minus(keywords.keys,
          callable.args.map {|var| var.first.underscored_name})
        if callable.splat && unknown_args.include?(callable.splat.underscored_name)
          raise Sass::SyntaxError.new("Argument $#{callable.splat.name} of #{downcase_desc} cannot be used as a named argument.")
        elsif unknown_args.any?
          description = unknown_args.length > 1 ? 'the following arguments:' : 'an argument named'
          raise Sass::SyntaxError.new("#{desc} doesn't have #{description} #{unknown_args.map {|name| "$#{name}"}.join ', '}.")
        end
      end
    rescue Sass::SyntaxError => keyword_exception
    end

    # If there's no splat, raise the keyword exception immediately. The actual
    # raising happens in the ensure clause at the end of this function.
    return if keyword_exception && !callable.splat

    if args.size > callable.args.size && !callable.splat
      takes = callable.args.size
      passed = args.size
      raise Sass::SyntaxError.new(
        "#{desc} takes #{takes} argument#{'s' unless takes == 1} " +
        "but #{passed} #{passed == 1 ? 'was' : 'were'} passed.")
    end

    splat_sep = :comma
    if splat
      args += splat.to_a
      splat_sep = splat.separator if splat.is_a?(Sass::Script::Value::List)
      # If the splat argument exists, there won't be any keywords passed in
      # manually, so we can safely overwrite rather than merge here.
      keywords = splat.keywords if splat.is_a?(Sass::Script::Value::ArgList)
    end

    keywords = keywords.dup
    env = Sass::Environment.new(callable.environment)
    callable.args.zip(args[0...callable.args.length]) do |(var, default), value|
      if value && keywords.include?(var.underscored_name)
        raise Sass::SyntaxError.new("#{desc} was passed argument $#{var.name} both by position and by name.")
      end

      value ||= keywords.delete(var.underscored_name)
      value ||= default && default.perform(env)
      raise Sass::SyntaxError.new("#{desc} is missing argument #{var.inspect}.") unless value
      env.set_local_var(var.name, value)
    end

    if callable.splat
      rest = args[callable.args.length..-1]
      arg_list = Sass::Script::Value::ArgList.new(rest, keywords.dup, splat_sep)
      arg_list.options = env.options
      env.set_local_var(callable.splat.name, arg_list)
    end

    yield env
  rescue Exception => e
  ensure
    # If there's a keyword exception, we don't want to throw it immediately,
    # because the invalid keywords may be part of a glob argument that should be
    # passed on to another function. So we only raise it if we reach the end of
    # this function *and* the keywords attached to the argument list glob object
    # haven't been accessed.
    #
    # The keyword exception takes precedence over any Sass errors, but not over
    # non-Sass exceptions.
    if keyword_exception &&
        !(arg_list && arg_list.keywords_accessed) &&
        (e.nil? || e.is_a?(Sass::SyntaxError))
      raise keyword_exception
    elsif e
      raise e
    end
  end

  protected

  def initialize(env)
    @environment = env
  end

  # If an exception is raised, this adds proper metadata to the backtrace.
  def visit(node)
    return super(node.dup) unless @environment
    @environment.stack.with_base(node.filename, node.line) {super(node.dup)}
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
    res = res.value if res.is_a?(Sass::Script::Value::String)
    if node.filename
      Sass::Util.sass_warn "#{node.filename}:#{node.line} DEBUG: #{res}"
    else
      Sass::Util.sass_warn "Line #{node.line} DEBUG: #{res}"
    end
    []
  end

  # Runs the child nodes once for each value in the list.
  def visit_each(node)
    list = node.list.perform(@environment)

    with_environment Sass::Environment.new(@environment) do
      list.to_a.map do |value|
        if node.vars.length == 1
          @environment.set_local_var(node.vars.first, value)
        else
          node.vars.zip(value.to_a) do |(var, sub_value)|
            @environment.set_local_var(var, sub_value || Sass::Script::Value::Null.new)
          end
        end
        node.children.map {|c| visit(c)}
      end.flatten
    end
  end

  # Runs SassScript interpolation in the selector,
  # and then parses the result into a {Sass::Selector::CommaSequence}.
  def visit_extend(node)
    parser = Sass::SCSS::StaticParser.new(run_interp(node.selector),
      node.filename, node.options[:importer], node.line)
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
    direction = from.to_i > to.to_i ? -1 : 1
    range = Range.new(direction * from.to_i, direction * to.to_i, node.exclusive)

    with_environment Sass::Environment.new(@environment) do
      range.map do |i|
        @environment.set_local_var(node.var,
          Sass::Script::Value::Number.new(direction * i, from.numerator_units, from.denominator_units))
        node.children.map { |c| visit(c) }
      end.flatten
    end
  end

  # Loads the function into the environment.
  def visit_function(node)
    env = Sass::Environment.new(@environment, node.options)
    @environment.set_local_function(node.name,
      Sass::Callable.new(node.name, node.args, node.splat, env, node.children, !:has_content, "function"))
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
      resolved_node = Sass::Tree::CssImportNode.resolved("url(#{path})")
      resolved_node.source_range = node.source_range
      return resolved_node
    end
    file = node.imported_file
    if @environment.stack.frames.any? {|f| f.is_import? && f.filename == file.options[:filename]}
      handle_import_loop!(node)
    end

    begin
      @environment.stack.with_import(node.filename, node.line) do
        root = file.to_tree
        Sass::Tree::Visitors::CheckNesting.visit(root)
        node.children = root.children.map {|c| visit(c)}.flatten
        node
      end
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:filename => node.imported_file.options[:filename])
      e.add_backtrace(:filename => node.filename, :line => node.line)
      raise e
    end
  end

  # Loads a mixin into the environment.
  def visit_mixindef(node)
    env = Sass::Environment.new(@environment, node.options)
    @environment.set_local_mixin(node.name,
      Sass::Callable.new(node.name, node.args, node.splat, env, node.children, node.has_content, "mixin"))
    []
  end

  # Runs a mixin.
  def visit_mixin(node)
    include_loop = true
    handle_include_loop!(node) if @environment.stack.frames.any? {|f| f.is_mixin? && f.name == node.name}
    include_loop = false

    @environment.stack.with_mixin(node.filename, node.line, node.name) do
      raise Sass::SyntaxError.new("Undefined mixin '#{node.name}'.") unless mixin = @environment.mixin(node.name)

      if node.children.any? && !mixin.has_content
        raise Sass::SyntaxError.new(%Q{Mixin "#{node.name}" does not accept a content block.})
      end

      args = node.args.map {|a| a.perform(@environment)}
      keywords = Sass::Util.map_hash(node.keywords) {|k, v| [k, v.perform(@environment)]}
      splat = node.splat.perform(@environment) if node.splat

      self.class.perform_arguments(mixin, args, keywords, splat) do |env|
        env.caller = Sass::Environment.new(@environment)
        env.content = node.children if node.has_children

        trace_node = Sass::Tree::TraceNode.from_node(node.name, node)
        with_environment(env) {trace_node.children = mixin.tree.map {|c| visit(c)}.flatten}
        trace_node
      end
    end
  rescue Sass::SyntaxError => e
    unless include_loop
      e.modify_backtrace(:mixin => node.name, :line => node.line)
      e.add_backtrace(:line => node.line)
    end
    raise e
  end

  def visit_content(node)
    return [] unless content = @environment.content
    @environment.stack.with_mixin(node.filename, node.line, '@content') do
      trace_node = Sass::Tree::TraceNode.from_node('@content', node)
      with_environment(@environment.caller) {trace_node.children = content.map {|c| visit(c.dup)}.flatten}
      trace_node
    end
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:mixin => '@content', :line => node.line)
    e.add_backtrace(:line => node.line)
    raise e
  end

  # Runs any SassScript that may be embedded in a property.
  def visit_prop(node)
    node.resolved_name = run_interp(node.name)
    val = node.value.perform(@environment)
    node.resolved_value = val.to_s
    node.value_source_range = val.source_range if val.source_range
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
    parser = Sass::SCSS::StaticParser.new(run_interp(node.rule),
      node.filename, node.options[:importer], node.line)
    node.parsed_rules ||= parser.parse_selector
    node.stack_trace = @environment.stack.to_s if node.options[:trace_selectors]
    with_environment Sass::Environment.new(@environment, node.options) do
      @environment.selector = node.parsed_rules
      node.children = node.children.map {|c| visit(c)}.flatten
    end
    node
  end

  # Loads the new variable value into the environment.
  def visit_variable(node)
    var = @environment.var(node.name)
    return [] if node.guarded && var && !var.null?
    val = node.expr.perform(@environment)
    if node.expr.source_range
      val.source_range = node.expr.source_range
    else
      val.source_range = node.source_range
    end
    @environment.set_var(node.name, val)
    []
  end

  # Prints the expression to STDERR with a stylesheet trace.
  def visit_warn(node)
    res = node.expr.perform(@environment)
    res = res.value if res.is_a?(Sass::Script::Value::String)
    msg = "WARNING: #{res}\n         "
    msg << @environment.stack.to_s.gsub("\n", "\n         ") << "\n"
    Sass::Util.sass_warn msg
    []
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
    parser = Sass::SCSS::StaticParser.new(run_interp(node.query),
      node.filename, node.options[:importer], node.line)
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
      parser = Sass::SCSS::StaticParser.new(run_interp(node.query),
        node.filename, node.options[:importer], node.line)
      node.resolved_query ||= parser.parse_media_query_list
    end
    yield
  end

  private

  def run_interp_no_strip(text)
    text.map do |r|
      next r if r.is_a?(String)
      val = r.perform(@environment)
      # Interpolated strings should never render with quotes
      next val.value if val.is_a?(Sass::Script::Value::String)
      val.to_s
    end.join
  end

  def run_interp(text)
    run_interp_no_strip(text).strip
  end

  def handle_include_loop!(node)
    msg = "An @include loop has been found:"
    content_count = 0
    mixins = @environment.stack.frames.select {|f| f.is_mixin?}.reverse.map {|f| f.name}.select do |name|
      if name == '@content'
        content_count += 1
        false
      elsif content_count > 0
        content_count -= 1
        false
      else
        true
      end
    end

    return unless mixins.include?(node.name)
    raise Sass::SyntaxError.new("#{msg} #{node.name} includes itself") if mixins.size == 1

    msg << "\n" << Sass::Util.enum_cons(mixins.reverse + [node.name], 2).map do |m1, m2|
      "    #{m1} includes #{m2}"
    end.join("\n")
    raise Sass::SyntaxError.new(msg)
  end

  def handle_import_loop!(node)
    msg = "An @import loop has been found:"
    files = @environment.stack.frames.select {|f| f.is_import?}.map {|f| f.filename}.compact
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
