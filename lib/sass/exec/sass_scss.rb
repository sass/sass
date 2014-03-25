module Sass::Exec
  # The `sass` and `scss` executables.
  class SassScss < Base
    attr_reader :default_syntax

    # @param args [Array<String>] The command-line arguments
    def initialize(args, default_syntax)
      super(args)
      @options[:for_engine] = {
        :load_paths => default_sass_path
      }
      @default_syntax = default_syntax
    end

    protected

    # Tells optparse how to parse the arguments.
    #
    # @param opts [OptionParser]
    # @comment
    #   rubocop:disable MethodLength
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
      opts.on('--poll', 'Check for file changes manually, rather than relying on the OS.',
                        'Only meaningful for --watch.') do
        @options[:poll] = true
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
      style_desc = 'Output style. Can be nested (default), compact, compressed, or expanded.'
      opts.on('-t', '--style NAME', style_desc) do |name|
        @options[:for_engine][:style] = name.to_sym
      end
      opts.on('--precision NUMBER_OF_DIGITS', Integer,
              "How many digits of precision to use when outputting decimal numbers." +
              "Defaults to #{Sass::Script::Value::Number.precision}.") do |precision|
        Sass::Script::Value::Number.precision = precision
      end
      opts.on('-q', '--quiet', 'Silence warnings and status messages during compilation.') do
        @options[:for_engine][:quiet] = true
      end
      opts.on('--compass', 'Make Compass imports available and load project configuration.') do
        @options[:compass] = true
      end
      opts.on('-g', '--debug-info',
              'Emit output that can be used by the FireSass Firebug plugin.') do
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
      opts.on('--cache-location PATH',
              'The path to put cached Sass files. Defaults to .sass-cache.') do |loc|
        @options[:for_engine][:cache_location] = loc
      end
      opts.on('-C', '--no-cache', "Don't cache to sassc files.") do
        @options[:for_engine][:cache] = false
      end
      opts.on('--sourcemap', 'Create sourcemap files next to the generated CSS files.') do
        @options[:sourcemap] = true
      end

      encoding_desc = if Sass::Util.ruby1_8?
                        'Does not work in ruby 1.8.'
                      else
                        'Specify the default encoding for Sass files.'
                      end
      opts.on('-E', '--default-encoding ENCODING', encoding_desc) do |encoding|
        if Sass::Util.ruby1_8?
          $stderr.puts "Specifying the encoding is not supported in ruby 1.8."
          exit 1
        else
          Encoding.default_external = encoding
        end
      end
    end
    # @comment
    #   rubocop:enable MethodLength

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
      @options[:for_engine][:css_filename] = @options[:output] if @options[:output].is_a?(String)
      @options[:for_engine][:sourcemap_filename] = @options[:sourcemap_filename]

      begin
        input = @options[:input]
        output = @options[:output]

        @options[:for_engine][:syntax] ||= :scss if input.is_a?(File) && input.path =~ /\.scss$/
        @options[:for_engine][:syntax] ||= @default_syntax
        engine =
          if input.is_a?(File) && !@options[:check_syntax]
            Sass::Engine.for_file(input.path, @options[:for_engine])
          else
            # We don't need to do any special handling of @options[:check_syntax] here,
            # because the Sass syntax checking happens alongside evaluation
            # and evaluation doesn't actually evaluate any code anyway.
            Sass::Engine.new(input.read, @options[:for_engine])
          end

        input.close if input.is_a?(File)

        if @options[:sourcemap]
          unless @options[:sourcemap_filename]
            raise "Can't generate a sourcemap for an input without a path."
          end

          relative_sourcemap_path = Sass::Util.pathname(@options[:sourcemap_filename]).
            relative_path_from(Sass::Util.pathname(@options[:output_filename]).dirname)
          rendered, mapping = engine.render_with_sourcemap(relative_sourcemap_path.to_s)
          write_output(rendered, output)
          write_output(mapping.to_json(
              :css_path => @options[:output_filename],
              :sourcemap_path => @options[:sourcemap_filename]) + "\n",
            @options[:sourcemap_filename])
        else
          write_output(engine.render, output)
        end
      rescue Sass::SyntaxError => e
        raise e if @options[:trace]
        raise e.sass_backtrace_str("standard input")
      ensure
        output.close if output.is_a? File
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
      Sass::Repl.new(@options).run
    end

    # @comment
    #   rubocop:disable MethodLength
    def watch_or_update
      require 'sass/plugin'
      Sass::Plugin.options.merge! @options[:for_engine]
      Sass::Plugin.options[:unix_newlines] = @options[:unix_newlines]
      Sass::Plugin.options[:poll] = @options[:poll]
      Sass::Plugin.options[:sourcemap] = @options[:sourcemap]

      if @options[:force]
        raise "The --force flag may only be used with --update." unless @options[:update]
        Sass::Plugin.options[:always_update] = true
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
      files.map! do |from, to|
        to ||= from.gsub(/\.[^.]*?$/, '.css')
        sourcemap = Util.sourcemap_name(to) if @options[:sourcemap]
        [from, to, sourcemap]
      end
      dirs.map! {|from, to| [from, to || from]}
      Sass::Plugin.options[:template_location] = dirs

      Sass::Plugin.on_updated_stylesheet do |_, css, sourcemap|
        [css, sourcemap].each do |file|
          next unless file
          puts_action :write, :green, file
        end
      end

      had_error = false
      Sass::Plugin.on_creating_directory {|dirname| puts_action :directory, :green, dirname}
      Sass::Plugin.on_deleting_css {|filename| puts_action :delete, :yellow, filename}
      Sass::Plugin.on_deleting_sourcemap {|filename| puts_action :delete, :yellow, filename}
      Sass::Plugin.on_compilation_error do |error, _, _|
        if error.is_a?(SystemCallError) && !@options[:stop_on_error]
          had_error = true
          puts_action :error, :red, error.message
          STDOUT.flush
          next
        end

        raise error unless error.is_a?(Sass::SyntaxError) && !@options[:stop_on_error]
        had_error = true
        puts_action :error, :red,
          "#{error.sass_filename} (Line #{error.sass_line}: #{error.message})"
        STDOUT.flush
      end

      if @options[:update]
        Sass::Plugin.update_stylesheets(files)
        exit 1 if had_error
        return
      end

      puts ">>> Sass is watching for changes. Press Ctrl-C to stop."

      Sass::Plugin.on_template_modified do |template|
        puts ">>> Change detected to: #{template}"
        STDOUT.flush
      end
      Sass::Plugin.on_template_created do |template|
        puts ">>> New template detected: #{template}"
        STDOUT.flush
      end
      Sass::Plugin.on_template_deleted do |template|
        puts ">>> Deleted template detected: #{template}"
        STDOUT.flush
      end

      Sass::Plugin.watch(files)
    end
    # @comment
    #   rubocop:enable MethodLength

    def colon_path?(path)
      !split_colon_path(path)[1].nil?
    end

    def split_colon_path(path)
      one, two = path.split(':', 2)
      if one && two && Sass::Util.windows? &&
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
      Sass::Util.glob(File.join(path, "*.s[ca]ss")).empty?
    end

    def default_sass_path
      return unless ENV['SASS_PATH']
      # The select here prevents errors when the environment's
      # load paths specified do not exist.
      ENV['SASS_PATH'].split(File::PATH_SEPARATOR).select {|d| File.directory?(d)}
    end
  end
end
