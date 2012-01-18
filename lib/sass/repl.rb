require 'readline'

module Sass
  # Runs a SassScript read-eval-print loop.
  # It presents a prompt on the terminal,
  # reads in SassScript expressions,
  # evaluates them,
  # and prints the result.
  class Repl
    # @param options [{Symbol => Object}] An options hash.
    def initialize(options = {})
      @options = options
    end

    # Starts the read-eval-print loop.
    def run
      environment = Environment.new
      @line = 0
      loop do
        @line += 1
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
        guarded = $3 == '||=' || $4
        val = Script::Parser.parse($3, @line, text.size - $3.size)

        unless guarded && environment.var(name)
          environment.set_var(name, val.perform(environment))
        end

        p environment.var(name)
      else
        p Script::Parser.parse(text, @line, 0).perform(environment)
      end
    rescue Sass::SyntaxError => e
      puts "SyntaxError: #{e.message}"
      if @options[:trace]
        e.backtrace.each do |e|
          puts "\tfrom #{e}"
        end
      end
    end
  end
end
