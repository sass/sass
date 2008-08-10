require 'strscan'
require 'sass/constant/operation'
require 'sass/constant/literal'

module Sass
  # This module contains various constant-script related functionality.
  module Constant
    # :stopdoc:

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
      ?, => :comma,
      ?+ => :plus,
      ?- => :minus,
      ?* => :times,
      ?/ => :div,
      ?% => :mod,
      ?& => :single_and,
      ?| => :single_or,
      ?= => :single_equals,
      CONSTANT_CHAR => :const_or_not,
      STRING_CHAR => :str,
      ESCAPE_CHAR => :esc
    }

    CONSTANT_CHARS = (SYMBOLS.keys + [ ?= ]).map {|c| Regexp.escape(c.chr) }.join

    # The regular expression used to parse constants
    MATCH = /^#{Regexp.escape(CONSTANT_CHAR.chr)}([^\s#{CONSTANT_CHARS}]+)\s*((?:\|\|)?=)\s*(.+)/

    # The regular expression used to validate constants without matching
    VALIDATE = /^#{Regexp.escape(CONSTANT_CHAR.chr)}[^\s#{CONSTANT_CHARS}]+$/

    # Order of operations hash
    ORDER = {
      :times => 1,
      :div => 1,
      :mod => 1,
      :plus => 2,
      :minus => 2,
      :comma => 3,
    }
    # First-order operations
    FIRST_ORDER = [:times, :div, :mod]

    # Second-order operations
    SECOND_ORDER = [:plus, :minus]

    class << self
      def resolve(*args)
        parse(*args).to_s
      end

      def parse(value, constants, line)
        operationalize(parenthesize(tokenize(value)), constants).perform
      rescue Sass::SyntaxError => e
        if e.message == "Constant arithmetic error"
          e.instance_eval do
            @message += ": #{value.dump}."
          end
        end
        e.sass_line = line
        raise e
      end

      private

      def tokenize(value)
        escaped = false
        is_string = false
        beginning_of_token = true
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
              if (symbol.nil? || symbol == :open || symbol == :const_or_not) &&
                  last && (!last.is_a?(Symbol) || last == :close)
                to_return << :concat
              end

              # String then open with no whitespace means funcall
              if symbol == :open && !str.empty?
                str = reset_str.call
                to_return << :funcall
              end

              # Time for a unary op!
              if ![nil, :open, :close, :const_or_not, :single_and, :single_or, :single_equals].include?(symbol) && beginning_of_token
                beginning_of_token = true
                to_return << :unary << symbol
                next
              end

              if [:single_and, :single_or, :single_equals].include?(symbol) && last == symbol
                to_return[-1] = symbol.to_s.gsub(/^single_/, '').to_sym
                next
              end

              if symbol == :single_equals && last == :const_or_not
                to_return[-1] = :not_equals
                to_return.slice!(-2) if to_return[-2] == :concat
                next
              end

              # Are we looking at an operator?
              if symbol && (symbol != :mod || str.empty?)
                str = reset_str.call
                beginning_of_token = symbol != :close
                to_return << symbol
                next
              end
            end
          end

          escaped = false
          beginning_of_token = false
          str << byte.chr
        end

        if is_string
          raise Sass::SyntaxError.new("Unterminated string: #{value.dump}.")
        end
        str = reset_str.call
        to_return
      end

      def parenthesize(value, return_after_expr = false)
        to_return = []

        while (token = value.shift) && token != :close
          case token
          when :open
            to_return << parenthesize(value)
          when :unary
            to_return << [value.shift, parenthesize(value, true)]
          when :const_or_not
            raise Sass::SyntaxError.new("Unterminated constant.") if value.first.nil?

            to_return << (value.first.is_a?(::String) ? [:const, value.shift] : [:not, parenthesize(value, true)])
          else
            to_return << token
          end

          return to_return.first if return_after_expr
        end
        return to_return
      end

      #--
      # TODO: Don't pass around original value;
      #       have Constant.parse automatically add it to exception.
      #++
      def operationalize(value, constants)
        value = [value] unless value.is_a?(Array)
        case value.length
        when 0
          Sass::Constant::Nil.new
        when 1
          value = value[0]
          if value.is_a? Array
            operationalize(value, constants)
          elsif value.is_a? Operation
            value
          else
            Literal.parse(value)
          end
        when 2
          case value[0]
          when :const
            Literal.parse(get_constant(value[1], constants))
          when ::Symbol
            UnaryOperation.new(operationalize(value[1], constants), value[0])
          else
            raise SyntaxError.new("Constant arithmetic error")
          end
        when 3
          Operation.new(operationalize(value[0], constants), operationalize(value[2], constants), value[1])
        else
          if ORDER[value[1]] && ORDER[value[3]] && ORDER[value[1]] > ORDER[value[3]]
            operationalize([value[0], value[1], operationalize(value[2..4], constants), *value[5..-1]], constants)
          else
            operationalize([operationalize(value[0..2], constants), *value[3..-1]], constants)
          end
        end
      end

      def get_constant(value, constants)
        to_return = constants[value]
        raise SyntaxError.new("Undefined constant: \"!#{value}\".") unless to_return
        to_return
      end
    end
    # :startdoc:
  end
end
