require 'readline'

module Sass
  module Repl
    class << self
      def run
        environment = Environment.new
        environment.set_var('important', Script::String.new('!important'))
        loop do
          unless text = Readline.readline('>> ')
            puts
            return
          end

          Readline::HISTORY << text
          parse_input(environment, text)
        end
      end

      private

      def parse_input(environment, text)
        case text
        when Script::MATCH
          name = $1
          guarded = $2 == '||='
          val = Script::Parser.parse($3)

          unless guarded && environment.var(name)
            environment.set_var(name, val.perform(environment))
          end

          p environment.var(name)
        else
          p Script::Parser.parse(text).perform(environment)
        end
      rescue Sass::SyntaxError => e
        puts "SyntaxError: #{e.message}"
      end
    end
  end
end
