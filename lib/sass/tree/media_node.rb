module Sass::Tree
  class MediaNode < DirectiveNode

    def bubbles?(parent)
      !parent.is_a?(MediaNode)
    end

    def merges?(parent)
      parent.is_a?(MediaNode)
    end

    def query
      rest.strip
    end

    def merge_with(node)
      unless node.is_a?(MediaNode)
        raise ArgumentError, "Cannot merge with #{node.inspect}"
      end
      n = MediaNode.new("@media (#{self.query}) and (#{node.query})")
      n.children = node.children
      n.options = options
      n
    end

  end
end