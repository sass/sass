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
    class Generic # :nodoc:
      def initialize(args)
        @args = args
        @options = {}
      end

      def parse!
        begin
          @opts = OptionParser.new(&(method(:set_opts).to_proc))
          @opts.parse!(@args)

          process_result
          
          @options
        rescue Exception => e
          raise e if e.is_a? SystemExit

          line = e.backtrace[0].scan(/:(.*)/)[0]
          puts "#{e.class} on line #{line}: #{e.message}"

          if @options[:trace]
            e.backtrace[1..-1].each { |t| puts "  #{t}" }
          else
            puts "  Use --trace to see traceback"
          end

          exit 1
        end
        exit 0
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

        opts.on('-s', '--stdio', 'Read input from standard input and print output to standard output') do
          @options[:input] = $stdin
          @options[:output] = $stdout
        end

        opts.on('--trace', :NONE, 'Show a full traceback on error') do
          @options[:trace] = true
        end

        opts.on_tail("-?", "-h", "--help", "Show this message") do
          puts opts
          exit
        end

        opts.on_tail("-v", "--version", "Print version") do
          puts("Haml " + File.read(File.dirname(__FILE__) + '/../../VERSION'))
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

        if input.is_a?(String) && !File.exists?(input)
          puts "File #{input} doesn't exist!"
          exit 1
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
    class HamlSass < Generic # :nodoc:
      private

      def set_opts(opts)
        opts.banner = <<END
Usage: #{@name.downcase} [options] (#{@name.downcase} file) (output file)

Description:
  Uses the #{@name} engine to parse the specified template
  and outputs the result to the specified file.

Options:
END
       
        opts.on('--rails RAILS_DIR', "Install Haml from the Gem to a Rails project") do |dir|
          original_dir = dir

          dir = File.join(dir, 'vendor', 'plugins')

          unless File.exists?(dir)
            puts "Directory #{dir} doesn't exist"
            exit
          end

          dir = File.join(dir, 'haml')

          if File.exists?(dir)
            puts "Directory #{dir} already exists."
            exit
          end

          begin
            Dir.mkdir(dir)
          rescue SystemCallError
            puts "Cannot create #{dir}"
            exit
          end

          File.open(File.join(dir, 'init.rb'), 'w') do |file|
            file.puts <<END
require 'rubygems'
require 'haml'
require 'haml/template'
require 'sass'
require 'sass/plugin'

ActionView::Base.register_template_handler('haml', Haml::Template)
Sass::Plugin.update_stylesheets
END
          end

          puts "Haml plugin added to #{original_dir}"
          exit
        end

        super
      end

      def process_result
        super
        require File.dirname(__FILE__) + "/../#{@name.downcase}"
      end
    end

    # A class encapsulating executable functionality
    # specific to Sass.
    class Sass < HamlSass # :nodoc:
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
    class Haml < HamlSass # :nodoc:
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

    # A class encapsulating executable functionality
    # specific to the html2haml executable.
    class HTML2Haml < Generic # :nodoc:
      def initialize(args)
        super

        @module_opts = {}

        begin
          require 'haml/html'
        rescue LoadError => err
          dep = err.message.scan(/^no such file to load -- (.*)/)[0]
          puts "Required dependency #{dep} not found!"
          exit 1
        end
      end

      def set_opts(opts)
        opts.banner = <<END
Usage: html2haml [options] (html file) (output file)

Description: Transforms an HTML file into corresponding Haml code.

Options:
END

        opts.on('-r', '--rhtml', 'Parse RHTML tags.') do
          @module_opts[:rhtml] = true
        end

        super
      end

      def process_result
        super

        input = @options[:input]
        output = @options[:output]

        output.write(::Haml::HTML.new(input, @module_opts).render)
      end
    end

    # A class encapsulating executable functionality
    # specific to the css2sass executable.
    class CSS2Sass < Generic # :nodoc:
      def initialize(args)
        super

        require 'sass/css'
      end

      def set_opts(opts)
        opts.banner = <<END
Usage: css2sass [options] (css file) (output file)

Description: Transforms a CSS file into corresponding Sass code.

Options:
END

        super
      end

      def process_result
        super

        input = @options[:input]
        output = @options[:output]

        output.write(::Sass::CSS.new(input).render)
      end
    end
  end
end
