require 'sass/tree/node'

module Sass::Tree
  class AttrNode < ValueNode
    attr_accessor :name
    
    def initialize(name, value, style)
      @name = name
      super(value, style)
    end
    
    def to_s(parent_name = nil)
      if value[-1] == ?;
        raise Sass::SyntaxError.new("Invalid attribute: #{declaration.dump} (This isn't CSS!)", @line)
      end
      real_name = name
      real_name = "#{parent_name}-#{real_name}" if parent_name

      if value.empty? && children.empty?
        raise Sass::SyntaxError.new("Invalid attribute: #{declaration.dump}", @line)
      end

      join_string = @style == :compact ? ' ' : "\n"
      to_return = ''
      if !value.empty?
        to_return << "#{real_name}: #{value};#{join_string}"
      end

      children.each do |kid|
        if @style == :compact
          to_return << "#{kid.to_s(real_name)} "
        else
          to_return << "#{kid.to_s(real_name)}\n"
        end
      end
      to_return << "\n" unless children.empty? || @style == :compact
      to_return[0...-1]
    end

    private

    def declaration
      ":#{name} #{value}"
    end

    def invalid_child?(child)
      if !child.is_a?(AttrNode)
        "Illegal nesting: Only attributes may be nested beneath attributes."
      end
    end
  end
end
