require 'sass/constant/operation'
require 'sass/constant/literal'

module Sass
  module Constant # :nodoc:
    # The character that begins a constant.
    CONSTANT_CHAR   = ?!

    # Whitespace characters
    WHITESPACE = [?\ , ?\t, ?\n, ?\r]
  
    # The character used to escape values
    ESCAPE_CHAR = ?\\

    # The character used to open and close strings
    STRING_CHAR = ?"
    
    # A mapping of syntactically-significant characters
    # to parsed symbols
    SYMBOLS = {
      ?( => :open,
      ?) => :close,
      ?+ => :plus,
      ?- => :minus,
      ?* => :times,
      ?/ => :div,
      ?% => :mod,
      STRING_CHAR => :str,
      ESCAPE_CHAR => :esc
    }

    # The regular expression used to parse constants
    MATCH = /^#{Regexp.escape(CONSTANT_CHAR.chr)}([^\s#{(SYMBOLS.keys + [ ?= ]).map {|c| Regexp.escape("#{c.chr}") }}]+)\s*=\s*(.+)/
    
    # First-order operations
    FIRST_ORDER = [:times, :div, :mod]
    
    # Second-order operations
    SECOND_ORDER = [:plus, :minus]
  
    class << self
      def parse(value, constants, line)
        begin
          operationalize(parenthesize(tokenize(value)), constants).to_s
        rescue Sass::SyntaxError => e
          if e.message == "Constant arithmetic error"
            e.instance_eval do
              @message += ": #{value.dump}"
            end
          end
          e.sass_line = line
          raise e
        end
      end
      
      private
      
      def tokenize(value)
        escaped = false
        is_string = false
        negative_okay = true
        str = ''
        to_return = []
        
        reset_str = Proc.new do
          to_return << str unless str.empty?
          ''
        end
        
        value.each_byte do |byte|
          unless escaped
            if byte == ESCAPE_CHAR
              escaped = true
              next
            end

            last = to_return[-1]

            # Do we need to open or close a string literal?
            if byte == STRING_CHAR
              is_string = !is_string

              # Adjacent strings should be concatenated
              if is_string && last && (!last.is_a?(Symbol) || last == :close)
                to_return << :concat
              end

              str = reset_str.call
              next
            end

            unless is_string

              # Are we looking at whitespace?
              if WHITESPACE.include?(byte)
                str = reset_str.call
                next
              end
            
              symbol = SYMBOLS[byte]

              # Adjacent values without an operator should be concatenated
              if (symbol.nil? || symbol == :open) &&
                  last && (!last.is_a?(Symbol) || last == :close)
                to_return << :concat
              end

              # Time for a unary minus!
              if negative_okay && symbol == :minus
                negative_okay = true
                to_return << :neg
                next
              end

              # Are we looking at an operator?
              if symbol && (str.empty? || symbol != :mod)
                str = reset_str.call
                negative_okay = true
                to_return << symbol
                next
              end
            end
          end
          
          escaped = false
          negative_okay = false
          str << byte.chr
        end
        
        if is_string
          raise Sass::SyntaxError.new("Unterminated string: #{value.dump}")
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
          elsif token == :neg
            if value[i + 1].nil?
              raise Sass::SyntaxError("Unterminated unary minus.")
            elsif value[i + 1] == :open
              to_return.push(*value[beginning...i])
              sub, i = parenthesize_helper(i + 2, value, value_len)
              beginning = i
              to_return << [:neg, sub]
            else
              to_return.push(*value[beginning...i])
              to_return << [:neg, value[i + 1]]
              beginning = i = i + 2
            end
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
      def operationalize(value, constants)
        value = [value] unless value.is_a?(Array)
        if value.length == 1
          value = value[0]
          if value.is_a? Array
            operationalize(value, constants)
          elsif value.is_a? Operation
            value
          else
            Literal.parse(insert_constant(value, constants))
          end
        elsif value.length == 2
          if value[0] == :neg
            Operation.new(Sass::Constant::Number.new('0'), operationalize(value[1], constants), :minus)
          else
            raise SyntaxError.new("Constant arithmetic error")
          end
        elsif value.length == 3
          Operation.new(operationalize(value[0], constants), operationalize(value[2], constants), value[1])
        else
          if SECOND_ORDER.include?(value[1]) && FIRST_ORDER.include?(value[3])
            operationalize([value[0], value[1], operationalize(value[2..4], constants), *value[5..-1]], constants)
          else
            operationalize([operationalize(value[0..2], constants), *value[3..-1]], constants)
          end
        end
      end
      
      def insert_constant(value, constants)
        to_return = value
        if value[0] == CONSTANT_CHAR
          to_return = constants[value[1..-1]]
          unless to_return
            raise SyntaxError.new("Undefined constant: \"#{value}\"")
          end
        end
        to_return
      end
    end
  end
end
