module Sass
  # A subclass of Ruby2Ruby that handles the custom `:comment` node,
  # used to insert source-position comments in the generated Ruby code.
  #
  # @private
  class Ruby2Ruby < ::Ruby2Ruby
    def process_comment(exp)
      exp.shift.to_s
    end

    def process_iter(exp) # :nodoc:
      iter = process exp.shift
      args = exp.shift
      body = exp.empty? ? nil : process(exp.shift)

      args = case args
             when 0 then
               " ||"
             else
               a = process(args)[1..-2]
               a = " |#{a}|" unless a.empty?
               a
             end

      b, e = if iter == "END" then
               [ "{", "}" ]
             else
               [ "do", "end" ]
             end

      iter.sub!(/\(\)$/, '')

      # Avoid a one-line lambda in case it contains a comment.

      result = []
      result << "#{iter} #{b}"
      result << args
      result << "\n"
      if body then
        result << indent(body.strip)
        result << "\n"
      end
      result << e
      result.join
    end
  end
end
