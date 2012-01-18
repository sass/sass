# A visitor for converting a static Sass tree into a static CSS tree.
class Sass::Tree::Visitors::Cssize < Sass::Tree::Visitors::Base
  # @param root [Tree::Node] The root node of the tree to visit.
  # @return [(Tree::Node, Sass::Util::SubsetMap)] The resulting tree of static nodes
  #   *and* the extensions defined for this tree
  def self.visit(root); super; end

  protected

  # Returns the immediate parent of the current node.
  # @return [Tree::Node]
  attr_reader :parent

  def initialize
    @extends = Sass::Util::SubsetMap.new
  end

  # If an exception is raised, this adds proper metadata to the backtrace.
  def visit(node)
    super(node.dup)
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Keeps track of the current parent node.
  def visit_children(parent)
    with_parent parent do
      parent.children = super.flatten
      parent
    end
  end

  # Runs a block of code with the current parent node
  # replaced with the given node.
  #
  # @param parent [Tree::Node] The new parent for the duration of the block.
  # @yield A block in which the parent is set to `parent`.
  # @return [Object] The return value of the block.
  def with_parent(parent)
    old_parent, @parent = @parent, parent
    yield
  ensure
    @parent = old_parent
  end

  # In Ruby 1.8, ensures that there's only one `@charset` directive
  # and that it's at the top of the document.
  #
  # @return [(Tree::Node, Sass::Util::SubsetMap)] The resulting tree of static nodes
  #   *and* the extensions defined for this tree
  def visit_root(node)
    yield

    # In Ruby 1.9 we can make all @charset nodes invisible
    # and infer the final @charset from the encoding of the final string.
    if Sass::Util.ruby1_8? && parent.nil?
      charset = node.children.find {|c| c.is_a?(Sass::Tree::CharsetNode)}
      node.children.reject! {|c| c.is_a?(Sass::Tree::CharsetNode)}
      node.children.unshift charset if charset
    end

    return node, @extends
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  # Registers an extension in the `@extends` subset map.
  def visit_extend(node)
    node.resolved_selector.members.each do |seq|
      if seq.members.size > 1
        raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: can't extend nested selectors")
      end

      sseq = seq.members.first
      if !sseq.is_a?(Sass::Selector::SimpleSequence)
        raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: invalid selector")
      end

      sel = sseq.members
      parent.resolved_rules.members.each do |seq|
        if !seq.members.last.is_a?(Sass::Selector::SimpleSequence)
          raise Sass::SyntaxError.new("#{seq} can't extend: invalid selector")
        end

        @extends[sel] = seq
      end
    end

    []
  end

  # Modifies exception backtraces to include the imported file.
  def visit_import(node)
    # Don't use #visit_children to avoid adding the import node to the list of parents.
    node.children.map {|c| visit(c)}.flatten
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.children.first.filename)
    e.add_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Bubbles the `@media` directive up through RuleNodes
  # and merges it with other `@media` directives.
  def visit_media(node)
    if parent.is_a?(Sass::Tree::RuleNode)
      new_rule = parent.dup
      new_rule.children = node.children
      node.children = with_parent(node) {Array(visit(new_rule))}
      # If the last child is actually the end of the group,
      # the parent's cssize will set it properly
      node.children.last.group_end = false unless node.children.empty?
    else
      yield
    end

    media = node.children.select {|c| c.is_a?(Sass::Tree::MediaNode)}
    node.children.reject! {|c| c.is_a?(Sass::Tree::MediaNode)}
    media.each do |n|
      n.query = node.query.map {|pq| n.query.map {|cq| "#{pq} and #{cq}"}}.flatten
    end
    (node.children.empty? ? [] : [node]) + media
  end

  # Asserts that all the mixin's children are valid in their new location.
  def visit_mixin(node)
    # Don't use #visit_children to avoid adding the mixin node to the list of parents.
    node.children.map {|c| visit(c)}.flatten
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:mixin => node.name, :filename => node.filename, :line => node.line)
    e.add_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  # Converts nested properties into flat properties
  # and updates the indentation of the prop node based on the nesting level.
  def visit_prop(node)
    if parent.is_a?(Sass::Tree::PropNode)
      node.resolved_name = "#{parent.resolved_name}-#{node.resolved_name}"
      node.tabs = parent.tabs + (parent.resolved_value.empty? ? 0 : 1) if node.style == :nested
    end

    yield

    result = node.children.dup
    if !node.resolved_value.empty? || node.children.empty?
      node.send(:check!)
      result.unshift(node)
    end

    result
  end

  # Resolves parent references and nested selectors,
  # and updates the indentation of the rule node based on the nesting level.
  def visit_rule(node)
    parent_resolved_rules = parent.is_a?(Sass::Tree::RuleNode) ? parent.resolved_rules : nil
    # It's possible for resolved_rules to be set if we've duplicated this node during @media bubbling
    node.resolved_rules ||= node.parsed_rules.resolve_parent_refs(parent_resolved_rules)

    yield

    rules = node.children.select {|c| c.is_a?(Sass::Tree::RuleNode) || c.is_a?(Sass::Tree::MediaNode)}
    props = node.children.reject {|c| c.is_a?(Sass::Tree::RuleNode) || c.is_a?(Sass::Tree::MediaNode) || c.invisible?}

    unless props.empty?
      node.children = props
      rules.each {|r| r.tabs += 1} if node.style == :nested
      rules.unshift(node)
    end

    rules.last.group_end = true unless parent.is_a?(Sass::Tree::RuleNode) || rules.empty?

    rules
  end
end
