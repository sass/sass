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
  end

  # If an exception is raised, this add proper metadata to the backtrace.
  def visit(node)
    super(node.dup)
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Keeps track of the current environment.
  def visit_children(parent)
    with_environment Sass::Environment.new(@environment) do
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
    @environment.options = node.options if @environment.options.nil? || @environment.options.empty?
    yield
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  # Removes this node from the tree if it's a silent comment.
  def visit_comment(node)
    return [] if node.invisible?
    check_for_loud_silent_comment node
    check_for_comment_interp node
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
    parser = Sass::SCSS::CssParser.new(run_interp(node.selector), node.filename, node.line)
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
    @environment.set_function(node.name,
      Sass::Callable.new(node.name, node.args, @environment, node.children))
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
      return Sass::Tree::DirectiveNode.new("@import url(#{path})")
    end
    file = node.imported_file
    handle_import_loop!(node) if @environment.files_in_use.include?(file.options[:filename])

    @environment.push_frame(:filename => node.filename, :line => node.line)
    root = file.to_tree
    Sass::Tree::Visitors::CheckNesting.visit(root)
    node.children = root.children.map {|c| visit(c)}.flatten
    node
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.imported_file.options[:filename])
    e.add_backtrace(:filename => node.filename, :line => node.line)
    raise e
  ensure
    @environment.pop_frame
  end

  # Loads a mixin into the environment.
  def visit_mixindef(node)
    @environment.set_mixin(node.name,
      Sass::Callable.new(node.name, node.args, @environment, node.children))
    []
  end

  # Runs a mixin.
  def visit_mixin(node)
    handle_include_loop!(node) if @environment.mixins_in_use.include?(node.name)

    original_env = @environment
    original_env.push_frame(:filename => node.filename, :line => node.line)
    original_env.prepare_frame(:mixin => node.name)
    raise Sass::SyntaxError.new("Undefined mixin '#{node.name}'.") unless mixin = @environment.mixin(node.name)

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
      raise Sass::SyntaxError.new("Mixin #{node.name} is missing parameter #{var.inspect}.") unless env.var(var.name)
      env
    end

    with_environment(environment) {node.children = mixin.tree.map {|c| visit(c)}.flatten}
    node
  rescue Sass::SyntaxError => e
    if original_env # Don't add backtrace info if this is an @include loop
      e.modify_backtrace(:mixin => node.name, :line => node.line)
      e.add_backtrace(:line => node.line)
    end
    raise e
  ensure
    original_env.pop_frame if original_env
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
      @environment.push_frame(:filename => node.filename, :line => node.line)
      node.stack_trace = @environment.stack_trace
      @environment.pop_frame
    end
    yield
  end

  # Loads the new variable value into the environment.
  def visit_variable(node)
    return [] if node.guarded && !@environment.var(node.name).nil?
    val = node.expr.perform(@environment)
    @environment.set_var(node.name, val)
    []
  end

  # Prints the expression to STDERR with a stylesheet trace.
  def visit_warn(node)
    @environment.push_frame(:filename => node.filename, :line => node.line)
    res = node.expr.perform(@environment)
    res = res.value if res.is_a?(Sass::Script::String)
    msg = "WARNING: #{res}\n         "
    msg << @environment.stack_trace.join("\n         ")
    # JRuby doesn't automatically add a newline for #warn
    msg << (RUBY_PLATFORM =~ /java/ ? "\n\n" : "\n")
    Sass::Util.sass_warn msg
    []
  ensure
    @environment.pop_frame
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
    if node.value['#{']
      if node.value =~ /^@import (?!url\()/
        Sass::Util.sass_warn <<WARNING
DEPRECATION WARNING on line #{node.line}#{" of #{node.filename}" if node.filename}:
@import directives using \#{} interpolation will need to use url() in Sass 3.2.
For example:

  @import url("http://\#{$url}/style.css");
WARNING
      end
      node.value = run_interp(Sass::Engine.parse_interp(node.value, node.line, 0, node.options))
    end
    yield
    node
  end

  private

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
    mixins = @environment.stack.map {|s| s[:mixin]}.compact
    raise Sass::SyntaxError.new("#{msg} #{node.name} includes itself") if mixins.size == 1

    mixins << node.name
    msg << "\n" << Sass::Util.enum_cons(mixins, 2).map do |m1, m2|
      "    #{m1} includes #{m2}"
    end.join("\n")
    raise Sass::SyntaxError.new(msg)
  end

  def handle_import_loop!(node)
    msg = "An @import loop has been found:"
    files = @environment.stack.map {|s| s[:filename]}.compact
    if node.filename == node.imported_file.options[:filename]
      raise Sass::SyntaxError.new("#{msg} #{node.filename} imports itself")
    end

    files << node.filename << node.imported_file.options[:filename]
    msg << "\n" << Sass::Util.enum_cons(files, 2).map do |m1, m2|
      "    #{m1} imports #{m2}"
    end.join("\n")
    raise Sass::SyntaxError.new(msg)
  end

  def check_for_loud_silent_comment(node)
    return unless node.loud && node.silent
    Sass::Util.sass_warn <<MESSAGE
WARNING:
On line #{node.line}#{" of '#{node.filename}'" if node.filename}
`//` comments will no longer be allowed to use the `!` flag in Sass 3.2.
Please change to `/*` comments.
MESSAGE
  end

  def check_for_comment_interp(node)
    return if node.loud
    node.value.each do |e|
      next unless e.is_a?(String)
      e.scan(/(\\*)#\{/) do |esc|
        Sass::Util.sass_warn <<MESSAGE if esc.first.size.even?
WARNING:
On line #{node.line}#{" of '#{node.filename}'" if node.filename}
Comments will evaluate the contents of interpolations (\#{ ... }) in Sass 3.2.
Please escape the interpolation by adding a backslash before the `#`.
MESSAGE
        return
      end
    end
  end
end
