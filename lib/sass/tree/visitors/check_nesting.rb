# A visitor for checking that all nodes are properly nested.
class Sass::Tree::Visitors::CheckNesting < Sass::Tree::Visitors::Base
  protected

  def visit(node)
    if error = (@parent && (
          try_send("invalid_#{node_name @parent}_child?", @parent, node) ||
          try_send("invalid_#{node_name node}_parent?", @parent, node))) ||
        (@real_parent && (
          try_send("invalid_#{node_name @real_parent}_real_child?", @real_parent, node) ||
          try_send("invalid_#{node_name node}_real_parent?", @real_parent, node)))
      raise Sass::SyntaxError.new(error)
    end
    super
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  PARENT_CLASSES = [ Sass::Tree::EachNode,   Sass::Tree::ForNode,   Sass::Tree::IfNode,
                     Sass::Tree::ImportNode, Sass::Tree::MixinNode, Sass::Tree::WhileNode]
  def visit_children(parent)
    old_parent = @parent
    @parent = parent unless is_any_of?(parent, PARENT_CLASSES)
    old_real_parent, @real_parent = @real_parent, parent
    super
  ensure
    @parent = old_parent
    @real_parent = old_real_parent
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

  def invalid_charset_parent?(parent, child)
    "@charset may only be used at the root of a document." unless parent.is_a?(Sass::Tree::RootNode)
  end

  INVALID_EXTEND_PARENTS = [Sass::Tree::RuleNode, Sass::Tree::MixinDefNode]
  def invalid_extend_parent?(parent, child)
    unless is_any_of?(parent, INVALID_EXTEND_PARENTS)
      "Extend directives may only be used within rules."
    end
  end

  def invalid_function_parent?(parent, child)
    "Functions may only be defined at the root of a document." unless parent.is_a?(Sass::Tree::RootNode)
  end

  INVALID_FUNCTION_CHILDREN = [
    Sass::Tree::CommentNode,  Sass::Tree::DebugNode, Sass::Tree::EachNode,
    Sass::Tree::ForNode,      Sass::Tree::IfNode,    Sass::Tree::ReturnNode,
    Sass::Tree::VariableNode, Sass::Tree::WarnNode,  Sass::Tree::WhileNode
  ]
  def invalid_function_child?(parent, child)
    unless is_any_of?(child, INVALID_FUNCTION_CHILDREN)
      "Functions can only contain variable declarations and control directives."
    end
  end

  INVALID_IMPORT_PARENTS = [
    Sass::Tree::IfNode,   Sass::Tree::ForNode, Sass::Tree::WhileNode,
    Sass::Tree::EachNode, Sass::Tree::MixinDefNode
  ]
  def invalid_import_parent?(parent, child)
    if is_any_of?(@real_parent, INVALID_IMPORT_PARENTS)
      return "Import directives may not be used within control directives or mixins."
    end
    return if parent.is_a?(Sass::Tree::RootNode)
    return "CSS import directives may only be used at the root of a document." if child.css_import?
    # If this is a nested @import, we need to make sure it doesn't have anything
    # that's legal at top-level but not in the current context (e.g. mixin defs).
    child.imported_file.to_tree.children.each {|c| visit(c)}
    nil
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => child.imported_file.options[:filename])
    e.add_backtrace(:filename => child.filename, :line => child.line)
    raise e
  end

  def invalid_import_real_parent?(parent, child)
    
  end

  def invalid_mixindef_parent?(parent, child)
    "Mixins may only be defined at the root of a document." unless parent.is_a?(Sass::Tree::RootNode)
  end

  INVALID_PROP_CHILDREN = [Sass::Tree::CommentNode, Sass::Tree::PropNode]
  def invalid_prop_child?(parent, child)
    unless is_any_of?(child, INVALID_PROP_CHILDREN)
      "Illegal nesting: Only properties may be nested beneath properties."
    end
  end

  INVALID_PROP_PARENTS = [Sass::Tree::RuleNode, Sass::Tree::PropNode,
                          Sass::Tree::MixinDefNode, Sass::Tree::DirectiveNode]
  def invalid_prop_parent?(parent, child)
    unless is_any_of?(parent, INVALID_PROP_PARENTS)
      "Properties are only allowed within rules, directives, or other properties." + child.pseudo_class_selector_message
    end
  end

  def invalid_return_parent?(parent, child)
    "@return may only be used within a function." unless parent.is_a?(Sass::Tree::FunctionNode)
  end

  private

  def is_any_of?(val, classes)
    for c in classes
      return true if val.is_a?(c)
    end
    return false
  end

  def try_send(method, *args, &block)
    return unless respond_to?(method)
    send(method, *args, &block)
  end
end

