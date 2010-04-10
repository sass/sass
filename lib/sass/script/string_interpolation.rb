module Sass::Script
  class StringInterpolation < Node
    def initialize(before, mid, after)
      @before = before
      @mid = mid
      @after = after
    end

    def inspect
      "(string_interpolation #{@before.inspect} #{@mid.inspect} #{@after.inspect})"
    end

    def to_sass(opts = {})
      # We can get rid of all of this when we remove the deprecated :equals context
      before_unquote, before_quote_char, before_str = parse_str(@before.to_sass(opts))
      after_unquote, after_quote_char, after_str = parse_str(@after.to_sass(opts))
      unquote = before_unquote || after_unquote ||
        (before_quote_char && !after_quote_char && !after_str.empty?) ||
        (!before_quote_char && after_quote_char && !before_str.empty?)
      quote_char =
        if before_quote_char && after_quote_char && before_quote_char != after_quote_char
          before_str.gsub!("\\'", "'")
          before_str.gsub!('"', "\\\"")
          after_str.gsub!("\\'", "'")
          after_str.gsub!('"', "\\\"")
          '"'
        else
          before_quote_char || after_quote_char
        end

      res = ""
      res << 'unquote(' if unquote
      res << quote_char if quote_char
      res << before_str
      res << '#{' << @mid.to_sass(opts) << '}'
      res << after_str
      res << quote_char if quote_char
      res << ')' if unquote
      res
    end

    def children
      [@before, @mid, @after].compact
    end

    protected

    def _perform(environment)
      res = ""
      res << @before.perform(environment).value
      val = @mid.perform(environment)
      res << (val.is_a?(Sass::Script::String) ? val.value : val.to_s)
      res << @after.perform(environment).value
      Sass::Script::String.new(res, :string)
    end

    def parse_str(str)
      case str
      when /^unquote\((["'])(.*)\1\)$/
        return true, $1, $2
      when '""'
        return false, nil, ""
      when /^(["'])(.*)\1$/
        return false, $1, $2
      else
        return false, nil, str
      end
    end
  end
end
