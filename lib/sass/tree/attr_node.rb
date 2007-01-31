require 'sass/tree/node'

module Sass::Tree
  class AttrNode < ValueNode
    attr_accessor :name
    
    def initialize(name, value)
      @name = name
      super(value)
    end
    
    def to_s(parent_name = nil)
      real_name = name
      real_name = "#{parent_name}-#{real_name}" if parent_name
      if children.size > 0
        to_return = String.new
        children.each { |kid| to_return += "#{kid.to_s(real_name)} " }
        to_return[0...-1]
      else
        if value.length < 1
          raise Sass::SyntaxError.new("Invalid attribute: \":#{name} #{value}\"", @line)
        end

        "#{real_name}: #{value};"
      end
    end
  end
end
