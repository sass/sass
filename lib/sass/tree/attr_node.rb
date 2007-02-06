require 'sass/tree/node'

module Sass::Tree
  class AttrNode < ValueNode
    attr_accessor :name
    
    def initialize(name, value, style)
      @name = name
      super(value, style)
    end
    
    def to_s(parent_name = nil)
      if name[-1] == ?: || value[-1] == ?;
        raise Sass::SyntaxError.new("Invalid attribute: #{declaration.dump} (This isn't CSS!)", @line)
      end
      real_name = name
      real_name = "#{parent_name}-#{real_name}" if parent_name
      if children.size > 0
        to_return = String.new
        children.each do |kid|
          if @style == :compact
            to_return << "#{kid.to_s(real_name)} "
          else
            to_return << "#{kid.to_s(real_name)}\n"
          end
        end
        to_return << "\n" unless @style == :compact
        to_return[0...-1]
      else
        if value.length < 1
          raise Sass::SyntaxError.new("Invalid attribute: #{declaration.dump}", @line)
        end

        "#{real_name}: #{value};"
      end
    end

    private

    def declaration
      ":#{name} #{value}"
    end
  end
end
