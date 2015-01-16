module Sass
  module Script
    module Helpers
      class << self
        def without_original(value)
          # TODO(nweiz): see if we can detect this ahead of time.
          return value unless value.is_a?(Sass::Script::Value::Number)
          value = value.dup
          value.original = nil
          value
        end

        def arg_hash(map)
          Sass::Util::NormalizedMap.new(Sass::Util.map_keys(map.to_h) do |key|
            next key.value if key.is_a?(Sass::Script::Value::String)
            raise Sass::SyntaxError.new("Variable keyword argument map must have string keys.\n" +
              "#{key.inspect} is not a string in #{map.inspect}.")
          end)
        end

        def maybe_warn_for_color(val, location, alternative)
          return unless val.is_a?(Sass::Script::Value::Color) && val.name
          Sass::Util.sass_warn <<MESSAGE
WARNING on #{location}:
You probably don't mean to use the color value `#{val}' in interpolation here.
It may end up represented as #{val.inspect}, which will likely produce invalid CSS.
Always quote color names when using them as strings (for example, "#{val}").
If you really want to use the color value here, use `#{alternative}'.
MESSAGE
        end

        def reformat_argument_error(ruby_name, sass_name, e)
          message = e.message

          # If this is a legitimate Ruby-raised argument error, re-raise it.
          # Otherwise, it's an error in the user's stylesheet, so wrap it.
          if Sass::Util.rbx?
            # Rubinius has a different error report string than vanilla Ruby. It
            # also doesn't put the actual method for which the argument error was
            # thrown in the backtrace, nor does it include `send`, so we look for
            # `_perform`.
            if e.message =~ /^method '([^']+)': given (\d+), expected (\d+)/
              error_name, given, expected = $1, $2, $3
              raise e if error_name != ruby_name || e.backtrace[0] !~ /:in `_perform'$/
              message = "wrong number of arguments (#{given} for #{expected})"
            end
          elsif Sass::Util.jruby?
            if Sass::Util.jruby1_6?
              should_maybe_raise = e.message =~ /^wrong number of arguments \((\d+) for (\d+)\)/ &&
                # The one case where JRuby does include the Ruby name of the function
                # is manually-thrown ArgumentErrors, which are indistinguishable from
                # legitimate ArgumentErrors. We treat both of these as
                # Sass::SyntaxErrors even though it can hide Ruby errors.
                e.backtrace[0] !~ /:in `(block in )?#{ruby_name}'$/
            else
              should_maybe_raise =
                e.message =~ /^wrong number of arguments calling `[^`]+` \((\d+) for (\d+)\)/
              given, expected = $1, $2
            end

            if should_maybe_raise
              # JRuby 1.7 includes __send__ before send and _perform.
              trace = e.backtrace.dup
              raise e if !Sass::Util.jruby1_6? && trace.shift !~ /:in `__send__'$/

              # JRuby (as of 1.7.2) doesn't put the actual method
              # for which the argument error was thrown in the backtrace, so we
              # detect whether our send threw an argument error.
              if !(trace[0] =~ /:in `send'$/ && trace[1] =~ /:in `_perform'$/)
                raise e
              elsif !Sass::Util.jruby1_6?
                # JRuby 1.7 doesn't use standard formatting for its ArgumentErrors.
                message = "wrong number of arguments (#{given} for #{expected})"
              end
            end
          elsif e.message =~ /^wrong number of arguments \(\d+ for \d+\)/ &&
              e.backtrace[0] !~ /:in `(block in )?#{ruby_name}'$/
            raise e
          end
          raise Sass::SyntaxError.new("#{message} for `#{sass_name}'")
        end
      end
    end
  end
end
