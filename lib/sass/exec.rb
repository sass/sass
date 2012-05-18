require 'optparse'
require 'fileutils'

module Sass
  # This module handles the various Sass executables (`sass` and `sass-convert`).
  module Exec
    # An abstract class that encapsulates the executable code for all three executables.
    class Generic
      # @param args [Array<String>] The command-line arguments
      def initialize(args)
        @args = args
        @options = {}
      end

      # Parses the command-line arguments and runs the executable.
      # Calls `Kernel#exit` at the end, so it never returns.
      #
      # @see #parse
      def parse!
        begin
          parse
        rescue Exception => e
          raise e if @options[:trace] || e.is_a?(SystemExit)

          $stderr.print "#{e.class}: " unless e.class == RuntimeError
          $stderr.puts "#{e.message}"
          $stderr.puts "  Use --trace for backtrace."
          exit 1
        end
        exit 0
      end

      # Parses the command-line arguments and runs the executable.
      # This does not handle exceptions or exit the program.
      #
      # @see #parse!
      def parse
        @opts = OptionParser.new(&method(:set_opts))
        @opts.parse!(@args)

        process_result

        @options
      end

      # @return [String] A description of the executable
      def to_s
        @opts.to_s
      end

      protected

      # Finds the line of the source template
      # on which an exception was raised.
      #
      # @param exception [Exception] The exception
      # @return [String] The line number
      def get_line(exception)
        # SyntaxErrors have weird line reporting
        # when there's trailing whitespace
        return (exception.message.scan(/:(\d+)/).first || ["??"]).first if exception.is_a?(::SyntaxError)
        (exception.backtrace[0].scan(/:(\d+)/).first || ["??"]).first
      end

      # Tells optparse how to parse the arguments
      # available for all executables.
      #
      # This is meant to be overridden by subclasses
      # so they can add their own options.
      #
      # @param opts [OptionParser]
      def set_opts(opts)
        opts.on('-s', '--stdin', :NONE, 'Read input from standard input instead of an input file') do
          @options[:input] = $stdin
        end

        opts.on('--trace', :NONE, 'Show a full traceback on error') do
          @options[:trace] = true
        end

        opts.on('--unix-newlines', 'Use Unix-style newlines in written files.') do
          @options[:unix_newlines] = true if ::Sass::Util.windows?
        end

        opts.on_tail("-?", "-h", "--help", "Show this message") do
          puts opts
          exit
        end

        opts.on_tail("-v", "--version", "Print version") do
          puts("Sass #{::Sass.version[:string]}")
          exit
        end
      end

      # Processes the options set by the command-line arguments.
      # In particular, sets `@options[:input]` and `@options[:output]`
      # to appropriate IO streams.
      #
      # This is meant to be overridden by subclasses
      # so they can run their respective programs.
      def process_result
        input, output = @options[:input], @options[:output]
        args = @args.dup
        input ||=
          begin
            filename = args.shift
            @options[:filename] = filename
            open_file(filename) || $stdin
          end
        output ||= open_file(args.shift, 'w') || $stdout

        @options[:input], @options[:output] = input, output
      end

      COLORS = { :red => 31, :green => 32, :yellow => 33 }

      # Prints a status message about performing the given action,
      # colored using the given color (via terminal escapes) if possible.
      #
      # @param name [#to_s] A short name for the action being performed.
      #   Shouldn't be longer than 11 characters.
      # @param color [Symbol] The name of the color to use for this action.
      #   Can be `:red`, `:green`, or `:yellow`.
      def puts_action(name, color, arg)
        return if @options[:for_engine][:quiet]
        printf color(color, "%11s %s\n"), name, arg
      end

      # Same as \{Kernel.puts}, but doesn't print anything if the `--quiet` option is set.
      #
      # @param args [Array] Passed on to \{Kernel.puts}
      def puts(*args)
        return if @options[:for_engine][:quiet]
        Kernel.puts(*args)
      end

      # Wraps the given string in terminal escapes
      # causing it to have the given color.
      # If terminal esapes aren't supported on this platform,
      # just returns the string instead.
      #
      # @param color [Symbol] The name of the color to use.
      #   Can be `:red`, `:green`, or `:yellow`.
      # @param str [String] The string to wrap in the given color.
      # @return [String] The wrapped string.
      def color(color, str)
        raise "[BUG] Unrecognized color #{color}" unless COLORS[color]

        # Almost any real Unix terminal will support color,
        # so we just filter for Windows terms (which don't set TERM)
        # and not-real terminals, which aren't ttys.
        return str if ENV["TERM"].nil? || ENV["TERM"].empty? || !STDOUT.tty?
        return "\e[#{COLORS[color]}m#{str}\e[0m"
      end

      private

      def open_file(filename, flag = 'r')
        return if filename.nil?
        flag = 'wb' if @options[:unix_newlines] && flag == 'w'
        File.open(filename, flag)
      end

      def handle_load_error(err)
        dep = err.message[/^no such file to load -- (.*)/, 1]
        raise err if @options[:trace] || dep.nil? || dep.empty?
        $stderr.puts <<MESSAGE
Required dependency #{dep} not found!
    Run "gem install #{dep}" to get it.
  Use --trace for backtrace.
MESSAGE
        exit 1
      end
    end

    # The `sass` executable.
    class Sass < Generic
      attr_reader :default_syntax

      # @param args [Array<String>] The command-line arguments
      def initialize(args)
        super
        @options[:for_engine] = {
          :load_paths => ['.'] + (ENV['SASSPATH'] || '').split(File::PATH_SEPARATOR)
        }
        @default_syntax = :sass
      end

      protected

      # Tells optparse how to parse the arguments.
      #
      # @param opts [OptionParser]
      def set_opts(opts)
        super

        opts.banner = <<END
Usage: #{default_syntax} [options] [INPUT] [OUTPUT]

Description:
  Converts SCSS or Sass files to CSS.

Options:
END

        if @default_syntax == :sass
          opts.on('--scss',
                  'Use the CSS-superset SCSS syntax.') do
            @options[:for_engine][:syntax] = :scss
          end
        else
          opts.on('--sass',
                  'Use the Indented syntax.') do
            @options[:for_engine][:syntax] = :sass
          end
        end
        opts.on('--watch', 'Watch files or directories for changes.',
                           'The location of the generated CSS can be set using a colon:',
                           "  #{@default_syntax} --watch input.#{@default_syntax}:output.css",
                           "  #{@default_syntax} --watch input-dir:output-dir") do
          @options[:watch] = true
        end
        opts.on('--update', 'Compile files or directories to CSS.',
                            'Locations are set like --watch.') do
          @options[:update] = true
        end
        opts.on('--stop-on-error', 'If a file fails to compile, exit immediately.',
                                   'Only meaningful for --watch and --update.') do
          @options[:stop_on_error] = true
        end
        opts.on('-f', '--force', 'Recompile all Sass files, even if the CSS file is newer.',
                                 'Only meaningful for --update.') do
          @options[:force] = true
        end
        opts.on('-c', '--check', "Just check syntax, don't evaluate.") do
          require 'stringio'
          @options[:check_syntax] = true
          @options[:output] = StringIO.new
        end
        opts.on('-t', '--style NAME',
                'Output style. Can be nested (default), compact, compressed, or expanded.') do |name|
          @options[:for_engine][:style] = name.to_sym
        end
        opts.on('--precision NUMBER_OF_DIGITS', Integer,
                'How many digits of precision to use when outputting decimal numbers. Defaults to 3.') do |precision|
          ::Sass::Script::Number.precision = precision
        end
        opts.on('-q', '--quiet', 'Silence warnings and status messages during compilation.') do
          @options[:for_engine][:quiet] = true
        end
        opts.on('--compass', 'Make Compass imports available and load project configuration.') do
          @options[:compass] = true
        end
        opts.on('-g', '--debug-info',
                'Emit extra information in the generated CSS that can be used by the FireSass Firebug plugin.') do
          @options[:for_engine][:debug_info] = true
        end
        opts.on('-l', '--line-numbers', '--line-comments',
                'Emit comments in the generated CSS indicating the corresponding source line.') do
          @options[:for_engine][:line_numbers] = true
        end
        opts.on('-i', '--interactive',
                'Run an interactive SassScript shell.') do
          @options[:interactive] = true
        end
        opts.on('-I', '--load-path PATH', 'Add a sass import path.') do |path|
          @options[:for_engine][:load_paths] << path
        end
        opts.on('-r', '--require LIB', 'Require a Ruby library before running Sass.') do |lib|
          require lib
        end
        opts.on('--cache-location PATH', 'The path to put cached Sass files. Defaults to .sass-cache.') do |loc|
          @options[:for_engine][:cache_location] = loc
        end
        opts.on('-C', '--no-cache', "Don't cache to sassc files.") do
          @options[:for_engine][:cache] = false
        end

        unless ::Sass::Util.ruby1_8?
          opts.on('-E encoding', 'Specify the default encoding for Sass files.') do |encoding|
            Encoding.default_external = encoding
          end
        end
      end

      # Processes the options set by the command-line arguments,
      # and runs the Sass compiler appropriately.
      def process_result
        require 'sass'

        if !@options[:update] && !@options[:watch] &&
            @args.first && colon_path?(@args.first)
          if @args.size == 1
            @args = split_colon_path(@args.first)
          else
            @options[:update] = true
          end
        end
        load_compass if @options[:compass]
        return interactive if @options[:interactive]
        return watch_or_update if @options[:watch] || @options[:update]
        super
        @options[:for_engine][:filename] = @options[:filename]

        begin
          input = @options[:input]
          output = @options[:output]

          @options[:for_engine][:syntax] ||= :scss if input.is_a?(File) && input.path =~ /\.scss$/
          @options[:for_engine][:syntax] ||= @default_syntax
          engine =
            if input.is_a?(File) && !@options[:check_syntax]
              ::Sass::Engine.for_file(input.path, @options[:for_engine])
            else
              # We don't need to do any special handling of @options[:check_syntax] here,
              # because the Sass syntax checking happens alongside evaluation
              # and evaluation doesn't actually evaluate any code anyway.
              ::Sass::Engine.new(input.read(), @options[:for_engine])
            end

          input.close() if input.is_a?(File)

          output.write(engine.render)
          output.close() if output.is_a? File
        rescue ::Sass::SyntaxError => e
          raise e if @options[:trace]
          raise e.sass_backtrace_str("standard input")
        end
      end

      private

      def load_compass
        begin
          require 'compass'
        rescue LoadError
          require 'rubygems'
          begin
            require 'compass'
          rescue LoadError
            puts "ERROR: Cannot load compass."
            exit 1
          end
        end
        Compass.add_project_configuration
        Compass.configuration.project_path ||= Dir.pwd
        @options[:for_engine][:load_paths] += Compass.configuration.sass_load_paths
      end

      def interactive
        require 'sass/repl'
        ::Sass::Repl.new(@options).run
      end

      def watch_or_update
        require 'sass/plugin'
        ::Sass::Plugin.options.merge! @options[:for_engine]
        ::Sass::Plugin.options[:unix_newlines] = @options[:unix_newlines]

        if @options[:force]
          raise "The --force flag may only be used with --update." unless @options[:update]
          ::Sass::Plugin.options[:always_update] = true
        end

        raise <<MSG if @args.empty?
What files should I watch? Did you mean something like:
    #{@default_syntax} --watch input.#{@default_syntax}:output.css
    #{@default_syntax} --watch input-dir:output-dir
MSG

        if !colon_path?(@args[0]) && probably_dest_dir?(@args[1])
          flag = @options[:update] ? "--update" : "--watch"
          err =
            if !File.exist?(@args[1])
              "doesn't exist"
            elsif @args[1] =~ /\.css$/
              "is a CSS file"
            end
          raise <<MSG if err
File #{@args[1]} #{err}.
    Did you mean: #{@default_syntax} #{flag} #{@args[0]}:#{@args[1]}
MSG
        end

        dirs, files = @args.map {|name| split_colon_path(name)}.
          partition {|i, _| File.directory? i}
        files.map! {|from, to| [from, to || from.gsub(/\.[^.]*?$/, '.css')]}
        dirs.map! {|from, to| [from, to || from]}
        ::Sass::Plugin.options[:template_location] = dirs

        ::Sass::Plugin.on_updated_stylesheet do |_, css|
          if File.exists? css
            puts_action :overwrite, :yellow, css
          else
            puts_action :create, :green, css
          end
        end

        had_error = false
        ::Sass::Plugin.on_creating_directory {|dirname| puts_action :directory, :green, dirname}
        ::Sass::Plugin.on_deleting_css {|filename| puts_action :delete, :yellow, filename}
        ::Sass::Plugin.on_compilation_error do |error, _, _|
          raise error unless error.is_a?(::Sass::SyntaxError) && !@options[:stop_on_error]
          had_error = true
          puts_action :error, :red, "#{error.sass_filename} (Line #{error.sass_line}: #{error.message})"
          STDOUT.flush
        end

        if @options[:update]
          ::Sass::Plugin.update_stylesheets(files)
          exit 1 if had_error
          return
        end

        puts ">>> Sass is watching for changes. Press Ctrl-C to stop."

        ::Sass::Plugin.on_template_modified do |template|
          puts ">>> Change detected to: #{template}"
          STDOUT.flush
        end
        ::Sass::Plugin.on_template_created do |template|
          puts ">>> New template detected: #{template}"
          STDOUT.flush
        end
        ::Sass::Plugin.on_template_deleted do |template|
          puts ">>> Deleted template detected: #{template}"
          STDOUT.flush
        end

        ::Sass::Plugin.watch(files)
      end

      def colon_path?(path)
        !split_colon_path(path)[1].nil?
      end

      def split_colon_path(path)
        one, two = path.split(':', 2)
        if one && two && ::Sass::Util.windows? &&
            one =~ /\A[A-Za-z]\Z/ && two =~ /\A[\/\\]/
          # If we're on Windows and we were passed a drive letter path,
          # don't split on that colon.
          one2, two = two.split(':', 2)
          one = one + ':' + one2
        end
        return one, two
      end

      # Whether path is likely to be meant as the destination
      # in a source:dest pair.
      def probably_dest_dir?(path)
        return false unless path
        return false if colon_path?(path)
        return ::Sass::Util.glob(File.join(path, "*.s[ca]ss")).empty?
      end
    end

    class Scss < Sass
      # @param args [Array<String>] The command-line arguments
      def initialize(args)
        super
        @default_syntax = :scss
      end
    end

    # The `sass-convert` executable.
    class SassConvert < Generic
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
          'The format to convert from. Can be css, scss, sass, less.',
          'By default, this is inferred from the input filename.',
          'If there is none, defaults to css.') do |name|
          @options[:from] = name.downcase.to_sym
          unless [:css, :scss, :sass, :less].include?(@options[:from])
            raise "Unknown format for sass-convert --from: #{name}"
          end
          try_less_note if @options[:from] == :less
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

        opts.on('--old', 'Output the old-style ":prop val" property syntax.',
                         'Only meaningful when generating Sass.') do
          @options[:for_tree][:old] = true
        end

        opts.on('-C', '--no-cache', "Don't cache to sassc files.") do
          @options[:for_engine][:read_cache] = false
        end

        unless ::Sass::Util.ruby1_8?
          opts.on('-E encoding', 'Specify the default encoding for Sass and CSS files.') do |encoding|
            Encoding.default_external = encoding
          end
        end

        super
      end

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
        raise "Error: '#{input.path}' is a directory (did you mean to use --recursive?)" if File.directory?(input)
        output = @options[:output]
        output = input if @options[:in_place]
        process_file(input, output)
      end

      private

      def process_directory
        unless input = @options[:input] = @args.shift
          raise "Error: directory required when using --recursive."
        end

        output = @options[:output] = @args.shift
        raise "Error: --from required when using --recursive." unless @options[:from]
        raise "Error: --to required when using --recursive." unless @options[:to]
        raise "Error: '#{@options[:input]}' is not a directory" unless File.directory?(@options[:input])
        if @options[:output] && File.exists?(@options[:output]) && !File.directory?(@options[:output])
          raise "Error: '#{@options[:output]}' is not a directory"
        end
        @options[:output] ||= @options[:input]

        from = @options[:from]
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
          output = @options[:in_place] ? input : open_file(output, "w")
          process_file(input, output)
        end
      end

      def process_file(input, output)
        if input.is_a?(File)
          @options[:from] ||=
            case input.path
            when /\.scss$/; :scss
            when /\.sass$/; :sass
            when /\.less$/; :less
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
            elsif @options[:from] == :less
              require 'sass/less'
              try_less_note
              input = input.read if input.is_a?(IO) && !input.is_a?(File) # Less is dumb
              Less::Engine.new(input).to_tree.to_sass_tree.send("to_#{@options[:to]}", @options[:for_tree])
            else
              if input.is_a?(File)
                ::Sass::Engine.for_file(input.path, @options[:for_engine])
              else
                ::Sass::Engine.new(input.read, @options[:for_engine])
              end.to_tree.send("to_#{@options[:to]}", @options[:for_tree])
            end
          end

        output = File.open(input.path, 'w') if @options[:in_place]
        output.write(out)
      rescue ::Sass::SyntaxError => e
        raise e if @options[:trace]
        file = " of #{e.sass_filename}" if e.sass_filename
        raise "Error on line #{e.sass_line}#{file}: #{e.message}\n  Use --trace for backtrace"
      rescue LoadError => err
        handle_load_error(err)
      end

      @@less_note_printed = false
      def try_less_note
        return if @@less_note_printed
        @@less_note_printed = true
        warn <<NOTE
* NOTE: Sass and Less are different languages, and they work differently.
* I'll do my best to translate, but some features -- especially mixins --
* should be checked by hand.
NOTE
      end
    end
  end
end
