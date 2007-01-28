require 'sass/constant/operation'
require 'sass/constant/literal'

module Sass
  module Constant
    # The character that begins a constant.
    CONSTANT_CHAR   = ?!

    # Whitespace characters
    WHITESPACE = [?\ , ?\t, ?\n, ?\r]
  
    # The character used to escape values
    ESCAPE_CHAR = ?\\
    
    # A mapping of syntactically-significant characters
    # to parsed symbols
    SYMBOLS = {
      '('[0] => :open,
      ')'[0] => :close,
      '+'[0] => :plus,
      '-'[0] => :minus,
      '*'[0] => :times,
      '/'[0] => :div,
      '%'[0] => :mod
    }

    # The regular expression used to parse constants
    MATCH = /^#{Regexp.escape(CONSTANT_CHAR.chr)}([^\s#{(SYMBOLS.keys + [ ESCAPE_CHAR, ?= ]).map {|c| Regexp.escape("#{c.chr}") }}]+)\s*=\s*(.+)/
    
    # First-order operations
    FIRST_ORDER = [:times, :div, :mod]
    
    # Second-order operations
    SECOND_ORDER = [:plus, :minus]
  
    class << self    
      def parse(value, constants, line)
        begin
          operationalize(parenthesize(tokenize(value)), value, constants).to_s
        rescue SyntaxError => e
          e.sass_line = line
          raise e
        end
      end
      
      private
      
      def tokenize(value)
        escaped = false
        negative_okay = true
        str = ''
        to_return = []
        
        reset_str = Proc.new do
          to_return << str unless str.empty?
          ''
        end
        
        value.each_byte do |byte|
          unless escaped
            if WHITESPACE.include?(byte)
              str = reset_str.call
              next
            end
            
            if byte == ESCAPE_CHAR
              escaped = true
              next
            end
            
            symbol = SYMBOLS[byte]
            if symbol && !(negative_okay && symbol == :minus)
              str = reset_str.call
              negative_okay = true
              to_return << symbol
              next
            end
          end
          
          escaped = false
          negative_okay = false
          str << byte.chr
        end
        
        str = reset_str.call
        to_return
      end
      
      def parenthesize(value)
        parenthesize_helper(0, value, value.length)[0]
      end
      
      def parenthesize_helper(i, value, value_len)
        to_return = []
        beginning = i
        token = value[i]
        
        while i < value_len && token != :close          
          if token == :open
            to_return.push(*value[beginning...i])
            sub, i = parenthesize_helper(i + 1, value, value_len)
            beginning = i
            to_return << sub
          else
            i += 1
          end
          
          token = value[i]
        end
        to_return.push(*value[beginning...i])
        return to_return, i + 1
      end
      
      #--
      # TODO: Don't pass around original value;
      #       have Constant.parse automatically add it to exception.
      #++
      def operationalize(value, original, constants)
        value = [value] unless value.is_a?(Array)
        length = value.length
        if length == 1
          value = value[0]
          if value.is_a? Operation
            value
          else
            Literal.parse(insert_constant(value, constants))
          end
        elsif length == 2
          raise SyntaxError.new("Constant arithmetic error: #{original}")
        elsif length == 3
          Operation.new(operationalize(value[0], original, constants), operationalize(value[2], original, constants), value[1])
        else
          unless length >= 5 && length % 2 == 1
            raise SyntaxError.new("Constant arithmetic error: #{original}")
          end
          if SECOND_ORDER.include?(value[1]) && FIRST_ORDER.include?(value[3])
            operationalize([value[0], value[1], operationalize(value[2..4], original, constants), *value[5..-1]], original, constants)
          else
            operationalize([operationalize(value[0..2], original, constants), *value[3..-1]], original, constants)
          end
        end
      end
      
      def insert_constant(value, constants)
        to_return = value
        if value[0] == CONSTANT_CHAR
          to_return = constants[value[1..-1]]
          unless to_return
            raise SyntaxError.new("Undefined constant: #{value}")
          end
        end
        to_return
      end
    end
  end
end
