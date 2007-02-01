require File.dirname(__FILE__) + '/../haml'
require 'optparse'

module Haml
  # This module contains code for working with the
  # haml, sass, and haml2html executables,
  # such as command-line parsing stuff.
  # It shouldn't need to be invoked by client code.
  module Exec # :nodoc:
    # A class that encapsulates the executable code
    # for all three executables.
    class Generic
      def initialize(args)
        @args = args
        @options = {}
      end

      def parse!
        @opts = OptionParser.new(&(method(:set_opts).to_proc))
        @opts.parse!(@args)

        process_result

        @options
      end

      def to_s
        @opts.to_s
      end
      
      private

      def set_opts(opts)
        opts.on('--stdin', :NONE, 'Read input from standard input instead of an input file') do
          @options[:input] = $stdin
        end

        opts.on('--stdout', :NONE, 'Print output to standard output instead of an output file') do
          @options[:output] = $stdout
        end

        opts.on_tail("-?", "-h", "--help", "Show this message") do
          puts opts
          exit
        end

        opts.on_tail("-v", "--version", "Print version") do
          puts("Haml " + File.read(File.dirname(__FILE__) + '/../VERSION'))
          exit
        end
      end

      def process_result
        input = @options[:input]
        output = @options[:output]

        if input
          output ||= ARGV[0]
        else
          input ||= ARGV[0]
          output ||= ARGV[1]
        end

        unless input && output
          puts @opts
          exit 1
        end

        [input, output].each do |file|
          if file.is_a?(String) && !File.exists?(file)
            puts "File #{file} doesn't exist!"
            exit 1
          end
        end

        unless input.is_a? IO
          input = File.open(input)
          input_file = true
        end

        unless output.is_a? IO
          output = File.open(output, "w")
          output_file = true
        end

        @options[:input] = input
        @options[:output] = output
      end
    end

    # A class encapsulating the executable functionality
    # specific to Haml and Sass.
    class HamlSass < Generic
      private

      def set_opts(opts)
        opts.banner = <<END
Usage: #{@name.downcase} [options] (template file) (output file)

Description:
  Uses the #{@name} engine to parse the specified template
  and outputs the result to the specified file.

Options:
END
       
        super
      end

      def process_result
        super
        require File.dirname(__FILE__) + "/../#{@name.downcase}"
      end
    end

    # A class encapsulating executable functionality
    # specific to Sass.
    class Sass < HamlSass
      def initialize(args)
        super
        @name = "Sass"
      end

      def process_result
        super
        input = @options[:input]
        output = @options[:output]

        template = input.read()
        input.close() if input.is_a? File
        result = ::Sass::Engine.new(template).render
        output.write(result)
        output.close() if output.is_a? File
      end
    end

    # A class encapsulating executable functionality
    # specific to Haml.
    class Haml < HamlSass
      def initialize(args)
        super
        @name = "Haml"
      end

      def process_result
        super
        input = @options[:input]
        output = @options[:output]

        template = input.read()
        input.close() if input.is_a? File
        result = ::Haml::Engine.new(template).to_html
        output.write(result)
        output.close() if output.is_a? File
      end
    end
  end
end
