# A visitor for copying the full structure of a Sass tree.
class Sass::Tree::Visitors::Grep < Sass::Tree::Visitors::Base
  protected

  def visit(node, &block)
    found = []
    found << node if yield(node)
    node.children.each do |child|
      found += visit(child, &block)
    end
    found
  end
end
