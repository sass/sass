# A visitor for performing selector inheritance on a static CSS tree.
#
# Destructively modifies the tree.
class Sass::Tree::Visitors::Extend < Sass::Tree::Visitors::Base
  # Performs the given extensions on the static CSS tree based in `root`, then
  # validates that all extends matched some selector.
  #
  # @param root [Tree::Node] The root node of the tree to visit.
  # @param extends [Sass::Util::SubsetMap{Selector::Simple =>
  #                                       Sass::Tree::Visitors::Cssize::Extend}]
  #   The extensions to perform on this tree.
  # @return [Object] The return value of \{#visit} for the root node.
  def self.visit(root, extends)
    return if extends.empty?
    new(extends).send(:visit, root)
    check_extends_fired! extends
  end

  protected

  def initialize(extends)
    @parent_directives = []
    @extends = extends
  end

  # If an exception is raised, this adds proper metadata to the backtrace.
  def visit(node)
    super(node)
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Keeps track of the current parent directives.
  def visit_children(parent)
    @parent_directives.push parent if parent.is_a?(Sass::Tree::DirectiveNode)
    super
  ensure
    @parent_directives.pop if parent.is_a?(Sass::Tree::DirectiveNode)
  end

  # Applies the extend to a single rule's selector.
  def visit_rule(node)
    node.resolved_rules = node.resolved_rules.do_extend(@extends, @parent_directives)
  end

  private

  def self.check_extends_fired!(extends)
    extends.each_value do |ex|
      next if ex.result == :succeeded || ex.node.optional?
      warn = "\"#{ex.extender}\" failed to @extend \"#{ex.target.join}\"."
      reason =
        if ex.result == :not_found
          "The selector \"#{ex.target.join}\" was not found."
        else
          "No selectors matching \"#{ex.target.join}\" could be unified with \"#{ex.extender}\"."
        end

      Sass::Util.sass_warn <<WARN
WARNING on line #{ex.node.line}#{" of #{ex.node.filename}" if ex.node.filename}: #{warn}
  #{reason}
  This will be an error in future releases of Sass.
  Use "@extend #{ex.target.join} !optional" if the extend should be able to fail.
WARN
    end
  end
end
