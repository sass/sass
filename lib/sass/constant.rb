require File.dirname(__FILE__) + '/../sass'
require 'sass/constant/operation'

module Sass
  module Constant
    # Whitespace characters
    WHITESPACE = [' '[0], "\t"[0], "\n"[0], "\r"[0]]
  
    # The character used to escape values
    ESCAPE_CHAR = '\\'[0]
    
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
    
    # First-order operations
    FIRST_ORDER = [:times, :div, :mod]
    
    # Second-order operations
    SECOND_ORDER = [:plus, :minus]
  
    class << self    
      def parse(value, constants)
        operationalize(parenthesize(tokenize(value)), value).perform(constants)
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
          i += 1
          token = value[i]
          
          if token == :open
            to_return.push(*value[beginning...i])
            sub, i = parenthesize_helper(i + 1, value, value_len)
            beginning = i
            token = value[i]
            to_return << sub
          end
        end
        to_return.push(*value[beginning...i])
        return to_return, i + 1
      end
      
      def operationalize(value, original)
        return Operation.new(value) unless value.is_a?(Array)
        length = value.length
        if length == 1
          Operation.new(value[0])
        elsif length == 2
          raise "Improperly formatted script:\n#{original}"
        elsif length == 3
          Operation.new(operationalize(value[0], original), operationalize(value[2], original), value[1])
        else
          raise "Improperly formatted script:\n#{original}" unless length >= 5 && length % 2 == 1
          if SECOND_ORDER.include?(value[1]) && FIRST_ORDER.include?(value[3])
            operationalize([value[1], value[2], operationalize(value[3..5]), *value[6..-1]], original)
          else
            operationalize([operationalize(value[0..2]), *value[4..-1]], original)
          end
        end
      end
    end
  end
end
