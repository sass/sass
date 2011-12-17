# A visitor for converting a Sass tree into a source string.
class Sass::Tree::Visitors::Convert < Sass::Tree::Visitors::Base
  # Runs the visitor on a tree.
  #
  # @param root [Tree::Node] The root node of the Sass tree.
  # @param options [{Symbol => Object}] An options hash (see {Sass::CSS#initialize}).
  # @param format [Symbol] `:sass` or `:scss`.
  # @return [String] The Sass or SCSS source for the tree.
  def self.visit(root, options, format)
    new(options, format).send(:visit, root)
  end

  protected

  def initialize(options, format)
    @options = options
    @format = format
    @tabs = 0
  end

  def visit_children(parent)
    @tabs += 1
    return @format == :sass ? "\n" : " {}\n" if parent.children.empty?
    (@format == :sass ? "\n" : " {\n") + super.join.rstrip + (@format == :sass ? "\n" : " }\n")
  ensure
    @tabs -= 1
  end

  # Ensures proper spacing between top-level nodes.
  def visit_root(node)
    Sass::Util.enum_cons(node.children + [nil], 2).map do |child, nxt|
      visit(child) +
        if nxt &&
            (child.is_a?(Sass::Tree::CommentNode) &&
              child.line + child.lines + 1 == nxt.line) ||
            (child.is_a?(Sass::Tree::ImportNode) && nxt.is_a?(Sass::Tree::ImportNode) &&
              child.line + 1 == nxt.line) ||
            (child.is_a?(Sass::Tree::VariableNode) && nxt.is_a?(Sass::Tree::VariableNode) &&
              child.line + 1 == nxt.line)
          ""
        else
          "\n"
        end
    end.join.rstrip + "\n"
  end

  def visit_charset(node)
    "#{tab_str}@charset \"#{node.name}\"#{semi}\n"
  end

  def visit_comment(node)
    value = node.value.map do |r|
      next r if r.is_a?(String)
      "\#{#{r.to_sass(@options)}}"
    end.join

    content = if @format == :sass
      content = value.gsub(/\*\/$/, '').rstrip
      if content =~ /\A[ \t]/
        # Re-indent SCSS comments like this:
        #     /* foo
        #   bar
        #       baz */
        content.gsub!(/^/, '   ')
        content.sub!(/\A([ \t]*)\/\*/, '/*\1')
      end

      content =
        unless content.include?("\n")
          content
        else
          content.gsub!(/\n( \*|\/\/)/, "\n  ")
          spaces = content.scan(/\n( *)/).map {|s| s.first.size}.min
          sep = node.silent ? "\n//" : "\n *"
          if spaces >= 2
            content.gsub(/\n  /, sep)
          else
            content.gsub(/\n#{' ' * spaces}/, sep)
          end
        end

      content.gsub!(/\A\/\*/, '//') if node.silent
      content.gsub!(/^/, tab_str)
      content.rstrip + "\n"
    else
      spaces = ('  ' * [@tabs - value[/^ */].size, 0].max)
      content = if node.silent
        value.gsub(/^[\/ ]\*/, '//').gsub(/ *\*\/$/, '')
      else
        value
      end.gsub(/^/, spaces) + "\n"
      content
    end
    if node.loud
      if node.silent
        content.gsub!(%r{^\s*(//!?)}, '//!')
      else
        content.sub!(%r{^\s*(/\*)}, '/*!')
      end
    end
    content
  end

  def visit_debug(node)
    "#{tab_str}@debug #{node.expr.to_sass(@options)}#{semi}\n"
  end

  def visit_directive(node)
    res = "#{tab_str}#{node.value}"
    return res + "#{semi}\n" unless node.has_children
    res + yield + "\n"
  end

  def visit_each(node)
    "#{tab_str}@each $#{dasherize(node.var)} in #{node.list.to_sass(@options)}#{yield}"
  end

  def visit_extend(node)
    "#{tab_str}@extend #{selector_to_src(node.selector).lstrip}#{semi}\n"
  end

  def visit_for(node)
    "#{tab_str}@for $#{dasherize(node.var)} from #{node.from.to_sass(@options)} " +
      "#{node.exclusive ? "to" : "through"} #{node.to.to_sass(@options)}#{yield}"
  end

  def visit_function(node)
    args = node.args.map do |v, d|
      d ? "#{v.to_sass(@options)}: #{d.to_sass(@options)}" : v.to_sass(@options)
    end.join(", ")

    "#{tab_str}@function #{dasherize(node.name)}(#{args})#{yield}"
  end

  def visit_if(node)
    name =
      if !@is_else; "if"
      elsif node.expr; "else if"
      else; "else"
      end
    str = "#{tab_str}@#{name}"
    str << " #{node.expr.to_sass(@options)}" if node.expr
    str << yield
    @is_else = true
    str << visit(node.else) if node.else
    str
  ensure
    @is_else = false
  end

  def visit_import(node)
    quote = @format == :scss ? '"' : ''
    "#{tab_str}@import #{quote}#{node.imported_filename}#{quote}#{semi}\n"
  end

  def visit_media(node)
    "#{tab_str}@media #{node.query.join(', ')}#{yield}"
  end

  def visit_mixindef(node)
    args =
      if node.args.empty?
        ""
      else
        '(' + node.args.map do |v, d|
          if d
            "#{v.to_sass(@options)}: #{d.to_sass(@options)}"
          else
            v.to_sass(@options)
          end
        end.join(", ") + ')'
      end
          
    "#{tab_str}#{@format == :sass ? '=' : '@mixin '}#{dasherize(node.name)}#{args}#{yield}"
  end

  def visit_mixin(node)
    unless node.args.empty? && node.keywords.empty?
      args = node.args.map {|a| a.to_sass(@options)}.join(", ")
      keywords = node.keywords.map {|k, v| "$#{dasherize(k)}: #{v.to_sass(@options)}"}.join(', ')
      arglist = "(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords})"
    end
    "#{tab_str}#{@format == :sass ? '+' : '@include '}#{dasherize(node.name)}#{arglist}#{semi}\n"
  end

  def visit_prop(node)
    res = tab_str + node.declaration(@options, @format)
    return res + semi + "\n" if node.children.empty?
    res + yield.rstrip + semi + "\n"
  end

  def visit_return(node)
    "#{tab_str}@return #{node.expr.to_sass(@options)}#{semi}\n"
  end

  def visit_rule(node)
    if @format == :sass
      name = selector_to_sass(node.rule)
      name = "\\" + name if name[0] == ?:
      name.gsub(/^/, tab_str) + yield
    elsif @format == :scss
      name = selector_to_scss(node.rule)
      res = name + yield
      if node.children.last.is_a?(Sass::Tree::CommentNode) && node.children.last.silent
        res.slice!(-3..-1)
        res << "\n" << tab_str << "}\n"
      end
      res
    end
  end

  def visit_variable(node)
    "#{tab_str}$#{dasherize(node.name)}: #{node.expr.to_sass(@options)}#{' !default' if node.guarded}#{semi}\n"
  end

  def visit_warn(node)
    "#{tab_str}@warn #{node.expr.to_sass(@options)}#{semi}\n"
  end

  def visit_while(node)
    "#{tab_str}@while #{node.expr.to_sass(@options)}#{yield}"
  end

  private

  def selector_to_src(sel)
    @format == :sass ? selector_to_sass(sel) : selector_to_scss(sel)
  end

  def selector_to_sass(sel)
    sel.map do |r|
      if r.is_a?(String)
        r.gsub(/(,)?([ \t]*)\n\s*/) {$1 ? "#{$1}#{$2}\n" : " "}
      else
        "\#{#{r.to_sass(@options)}}"
      end
    end.join
  end

  def selector_to_scss(sel)
    sel.map {|r| r.is_a?(String) ? r : "\#{#{r.to_sass(@options)}}"}.
      join.gsub(/^[ \t]*/, tab_str).gsub(/[ \t]*$/, '')
  end

  def semi
    @format == :sass ? "" : ";"
  end

  def tab_str
    '  ' * @tabs
  end

  def dasherize(s)
    if @options[:dasherize]
      s.gsub('_', '-')
    else
      s
    end
  end
end
