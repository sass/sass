require 'optparse'
require 'fileutils'

module Sass::Exec
  # The `sass-convert` executable.
  class SassConvert < Base
    # @param args [Array<String>] The command-line arguments
    def initialize(args)
      super
      require 'sass'
      @options[:for_tree] = {}
      @options[:for_engine] = {:cache => false, :read_cache => true}
    end

    # Tells optparse how to parse the arguments.
    #
    # @param opts [OptionParser]
    # @comment
    #   rubocop:disable MethodLength
    def set_opts(opts)
      opts.banner = <<END
Usage: sass-convert [options] [INPUT] [OUTPUT]

Description:
  Converts between CSS, Sass, and SCSS files.
  E.g. converts from SCSS to Sass,
  or converts from CSS to SCSS (adding appropriate nesting).

Options:
END

      opts.on('-F', '--from FORMAT',
        'The format to convert from. Can be css, scss, sass.',
        'By default, this is inferred from the input filename.',
        'If there is none, defaults to css.') do |name|
        @options[:from] = name.downcase.to_sym
        raise "sass-convert no longer supports LessCSS." if @options[:from] == :less
        unless [:css, :scss, :sass].include?(@options[:from])
          raise "Unknown format for sass-convert --from: #{name}"
        end
      end

      opts.on('-T', '--to FORMAT',
        'The format to convert to. Can be scss or sass.',
        'By default, this is inferred from the output filename.',
        'If there is none, defaults to sass.') do |name|
        @options[:to] = name.downcase.to_sym
        unless [:scss, :sass].include?(@options[:to])
          raise "Unknown format for sass-convert --to: #{name}"
        end
      end

      opts.on('-R', '--recursive',
        'Convert all the files in a directory. Requires --from and --to.') do
        @options[:recursive] = true
      end

      opts.on('-i', '--in-place',
        'Convert a file to its own syntax.',
        'This can be used to update some deprecated syntax.') do
        @options[:in_place] = true
      end

      opts.on('--dasherize', 'Convert underscores to dashes') do
        @options[:for_tree][:dasherize] = true
      end

      opts.on('--indent NUM',
        'How many spaces to use for each level of indentation. Defaults to 2.',
        '"t" means use hard tabs.') do |indent|

        if indent == 't'
          @options[:for_tree][:indent] = "\t"
        else
          @options[:for_tree][:indent] = " " * indent.to_i
        end
      end

      opts.on('--old', 'Output the old-style ":prop val" property syntax.',
                       'Only meaningful when generating Sass.') do
        @options[:for_tree][:old] = true
      end

      opts.on('-C', '--no-cache', "Don't cache to sassc files.") do
        @options[:for_engine][:read_cache] = false
      end

      unless ::Sass::Util.ruby1_8?
        opts.on('-E encoding',
                'Specify the default encoding for Sass and CSS files.') do |encoding|
          Encoding.default_external = encoding
        end
      end

      super
    end
    # @comment
    #   rubocop:enable MethodLength

    # Processes the options set by the command-line arguments,
    # and runs the CSS compiler appropriately.
    def process_result
      require 'sass'

      if @options[:recursive]
        process_directory
        return
      end

      super
      input = @options[:input]
      if File.directory?(input)
        raise "Error: '#{input.path}' is a directory (did you mean to use --recursive?)"
      end
      output = @options[:output]
      output = input if @options[:in_place]
      process_file(input, output)
    end

    private

    def process_directory
      unless (input = @options[:input] = @args.shift)
        raise "Error: directory required when using --recursive."
      end

      output = @options[:output] = @args.shift
      raise "Error: --from required when using --recursive." unless @options[:from]
      raise "Error: --to required when using --recursive." unless @options[:to]
      unless File.directory?(@options[:input])
        raise "Error: '#{@options[:input]}' is not a directory"
      end
      if @options[:output] && File.exists?(@options[:output]) &&
        !File.directory?(@options[:output])
        raise "Error: '#{@options[:output]}' is not a directory"
      end
      @options[:output] ||= @options[:input]

      if @options[:to] == @options[:from] && !@options[:in_place]
        fmt = @options[:from]
        raise "Error: converting from #{fmt} to #{fmt} without --in-place"
      end

      ext = @options[:from]
      ::Sass::Util.glob("#{@options[:input]}/**/*.#{ext}") do |f|
        output =
          if @options[:in_place]
            f
          elsif @options[:output]
            output_name = f.gsub(/\.(c|sa|sc|le)ss$/, ".#{@options[:to]}")
            output_name[0...@options[:input].size] = @options[:output]
            output_name
          else
            f.gsub(/\.(c|sa|sc|le)ss$/, ".#{@options[:to]}")
          end

        unless File.directory?(File.dirname(output))
          puts_action :directory, :green, File.dirname(output)
          FileUtils.mkdir_p(File.dirname(output))
        end
        puts_action :convert, :green, f
        if File.exists?(output)
          puts_action :overwrite, :yellow, output
        else
          puts_action :create, :green, output
        end

        input = open_file(f)
        process_file(input, output)
      end
    end

    def process_file(input, output)
      if input.is_a?(File)
        @options[:from] ||=
          case input.path
          when /\.scss$/; :scss
          when /\.sass$/; :sass
          when /\.less$/; raise "sass-convert no longer supports LessCSS."
          when /\.css$/; :css
          end
      elsif @options[:in_place]
        raise "Error: the --in-place option requires a filename."
      end

      if output.is_a?(File)
        @options[:to] ||=
          case output.path
          when /\.scss$/; :scss
          when /\.sass$/; :sass
          end
      end

      @options[:from] ||= :css
      @options[:to] ||= :sass
      @options[:for_engine][:syntax] = @options[:from]

      out =
        ::Sass::Util.silence_sass_warnings do
          if @options[:from] == :css
            require 'sass/css'
            ::Sass::CSS.new(input.read, @options[:for_tree]).render(@options[:to])
          else
            if input.is_a?(File)
              ::Sass::Engine.for_file(input.path, @options[:for_engine])
            else
              ::Sass::Engine.new(input.read, @options[:for_engine])
            end.to_tree.send("to_#{@options[:to]}", @options[:for_tree])
          end
        end

      output = input.path if @options[:in_place]
      write_output(out, output)
    rescue ::Sass::SyntaxError => e
      raise e if @options[:trace]
      file = " of #{e.sass_filename}" if e.sass_filename
      raise "Error on line #{e.sass_line}#{file}: #{e.message}\n  Use --trace for backtrace"
    rescue LoadError => err
      handle_load_error(err)
    end
  end
end
