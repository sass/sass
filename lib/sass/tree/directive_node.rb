module Sass::Tree
  # A static node representing an unproccessed Sass `@`-directive.
  # Directives known to Sass, like `@for` and `@debug`,
  # are handled by their own nodes;
  # only CSS directives like `@media` and `@font-face` become {DirectiveNode}s.
  #
  # `@import` is a bit of a weird case;
  # it becomes an {ImportNode}.
  #
  # @see Sass::Tree
  class DirectiveNode < Node
    # The text of the directive, `@` and all.
    #
    # @return [String]
    attr_accessor :value

    # @param value [String] See \{#value}
    def initialize(value)
      @value = value
      super()
    end

    def name
      value.split(/\s/, 2).first
    end

    def rest
      value.split(/\s+/, 2).last
    end

    def media?
      name == "@media"
    end

    def bubbles?(parent)
      r = media? && !(parent.is_a?(DirectiveNode) && parent.media?)
      r
    end

    def merges?(parent)
      r = media? && parent.is_a?(DirectiveNode) && parent.media?
      r
    end

    def merge_with(node)
      unless node.is_a?(DirectiveNode) && node.media?
        raise ArgumentError, "Cannot merge with #{node.inspect}"
      end
      query1 = rest.strip
      query1 = query1[1..-2] if query1[0] == ?( && query1[query1.size - 1] == ?)
      query2 = node.rest.strip
      query2 = query2[1..-2] if query2[0] == ?( && query2[query2.size - 1] == ?)
      n = DirectiveNode.new("@media (#{rest}) and (#{node.rest})")
      n.children = node.children
      n.options = options
      n
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      res = "#{'  ' * tabs}#{value}"
      return res + "#{semi fmt}\n" unless has_children
      res + children_to_src(tabs, opts, fmt) + "\n"
    end

    # Computes the CSS for the directive.
    #
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String] The resulting CSS
    def _to_s(tabs)
      return value + ";" unless has_children
      return value + " {}" if children.empty?
      result = if style == :compressed
                 "#{value}{"
               else
                 "#{'  ' * (tabs - 1)}#{value} {" + (style == :compact ? ' ' : "\n")
               end
      was_prop = false
      first = true
      children.each do |child|
        next if child.invisible?
        if style == :compact
          if child.is_a?(PropNode)
            result << "#{child.to_s(first || was_prop ? 1 : tabs + 1)} "
          else
            if was_prop
              result[-1] = "\n"
            end
            rendered = child.to_s(tabs + 1).dup
            rendered = rendered.lstrip if first
            result << rendered.rstrip + "\n"
          end
          was_prop = child.is_a?(PropNode)
          first = false
        elsif style == :compressed
          result << (was_prop ? ";#{child.to_s(1)}" : child.to_s(1))
          was_prop = child.is_a?(PropNode)
        else
          result << child.to_s(tabs + 1) + "\n"
        end
      end
      result.rstrip + if style == :compressed
                        "}"
                      else
                        (style == :expanded ? "\n" : " ") + "}\n"
                      end
    end
  end
end
