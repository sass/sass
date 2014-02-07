# A visitor for checking that all nodes are properly nested.
class Sass::Tree::Visitors::CheckNesting < Sass::Tree::Visitors::Base
  protected

  def initialize
    @parents = []
  end

  def visit(node)
    if (error = @parent && (
        try_send(@parent.class.invalid_child_method_name, @parent, node) ||
        try_send(node.class.invalid_parent_method_name, @parent, node)))
      raise Sass::SyntaxError.new(error)
    end
    super
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  CONTROL_NODES = [Sass::Tree::EachNode, Sass::Tree::ForNode, Sass::Tree::IfNode,
                   Sass::Tree::WhileNode, Sass::Tree::TraceNode]
  SCRIPT_NODES = [Sass::Tree::ImportNode] + CONTROL_NODES
  def visit_children(parent)
    old_parent = @parent
    @parent = parent unless is_any_of?(parent, SCRIPT_NODES) ||
      (parent.bubbles? && !old_parent.is_a?(Sass::Tree::RootNode))
    @parents.push parent
    super
  ensure
    @parent = old_parent
    @parents.pop
  end

  def visit_root(node)
    yield
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  def visit_import(node)
    yield
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.children.first.filename)
    e.add_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  def visit_mixindef(node)
    @current_mixin_def, old_mixin_def = node, @current_mixin_def
    yield
  ensure
    @current_mixin_def = old_mixin_def
  end

  def invalid_content_parent?(parent, child)
    if @current_mixin_def
      @current_mixin_def.has_content = true
      nil
    else
      "@content may only be used within a mixin."
    end
  end

  def invalid_charset_parent?(parent, child)
    "@charset may only be used at the root of a document." unless parent.is_a?(Sass::Tree::RootNode)
  end

  VALID_EXTEND_PARENTS = [Sass::Tree::RuleNode, Sass::Tree::MixinDefNode, Sass::Tree::MixinNode]
  def invalid_extend_parent?(parent, child)
    unless is_any_of?(parent, VALID_EXTEND_PARENTS)
      return "Extend directives may only be used within rules."
    end
  end

  INVALID_IMPORT_PARENTS = CONTROL_NODES +
    [Sass::Tree::MixinDefNode, Sass::Tree::MixinNode]
  def invalid_import_parent?(parent, child)
    unless (@parents.map {|p| p.class} & INVALID_IMPORT_PARENTS).empty?
      return "Import directives may not be used within control directives or mixins."
    end
    return if parent.is_a?(Sass::Tree::RootNode)
    return "CSS import directives may only be used at the root of a document." if child.css_import?
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => child.imported_file.options[:filename])
    e.add_backtrace(:filename => child.filename, :line => child.line)
    raise e
  end

  def invalid_mixindef_parent?(parent, child)
    unless (@parents.map {|p| p.class} & INVALID_IMPORT_PARENTS).empty?
      return "Mixins may not be defined within control directives or other mixins."
    end
  end

  def invalid_function_parent?(parent, child)
    unless (@parents.map {|p| p.class} & INVALID_IMPORT_PARENTS).empty?
      return "Functions may not be defined within control directives or other mixins."
    end
  end

  VALID_FUNCTION_CHILDREN = [
    Sass::Tree::CommentNode,  Sass::Tree::DebugNode, Sass::Tree::ReturnNode,
    Sass::Tree::VariableNode, Sass::Tree::WarnNode
  ] + CONTROL_NODES
  def invalid_function_child?(parent, child)
    unless is_any_of?(child, VALID_FUNCTION_CHILDREN)
      "Functions can only contain variable declarations and control directives."
    end
  end

  VALID_PROP_CHILDREN =  CONTROL_NODES + [Sass::Tree::CommentNode,
                                          Sass::Tree::PropNode,
                                          Sass::Tree::MixinNode]
  def invalid_prop_child?(parent, child)
    unless is_any_of?(child, VALID_PROP_CHILDREN)
      "Illegal nesting: Only properties may be nested beneath properties."
    end
  end

  VALID_PROP_PARENTS = [Sass::Tree::RuleNode, Sass::Tree::PropNode,
                        Sass::Tree::MixinDefNode, Sass::Tree::DirectiveNode,
                        Sass::Tree::MixinNode]
  def invalid_prop_parent?(parent, child)
    unless is_any_of?(parent, VALID_PROP_PARENTS)
      "Properties are only allowed within rules, directives, mixin includes, or other properties." +
        child.pseudo_class_selector_message
    end
  end

  def invalid_return_parent?(parent, child)
    "@return may only be used within a function." unless parent.is_a?(Sass::Tree::FunctionNode)
  end

  private

  def is_any_of?(val, classes)
    classes.each do |c|
      return true if val.is_a?(c)
    end
    false
  end

  def try_send(method, *args)
    return unless respond_to?(method, true)
    send(method, *args)
  end
end
