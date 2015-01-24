test_dir = File.dirname(__FILE__)
$:.unshift test_dir unless $:.include?(test_dir)

class MiniTest::Test
  def absolutize(file)
    File.expand_path("#{File.dirname(__FILE__)}/#{file}")
  end

  def eval_sass_script(str, options = {})
    options = Sass::Engine::DEFAULT_OPTIONS.merge options
    munge_filename options
    options = Sass::Engine.normalize_options(options)
    to_sexp = Sass::Tree::Visitors::ToSexp.new(options)
    # TODO: this is a hack
    to_sexp.environment = Sass::Environment.new
    sexp = Sass::Script::Parser.parse(str, 0, 0).to_sexp(to_sexp)
    ruby = Sass::Ruby2Ruby.new.process(sexp)
    mapper = Sass::RubyMapper.new(ruby)
    environment = Sass::RuntimeEnvironment.new(options)
    environment.selector = options[:selector]
    eval_context = Sass::Script::Functions::EvaluationContext.new(
      environment, mapper, to_sexp.fn_signatures, to_sexp.mx_signatures)
    (class << eval_context; self; end).send(:define_method, :_s_env) {environment}
    (class << eval_context; self; end).send(:define_method, :_s_importer) {nil}
    with_thread_options(options) {eval_context.instance_eval(ruby)}
  end

  def with_thread_options(options)
    begin
      old_options = Thread.current[:options]
      Thread.current[:options] = options
      yield
    ensure
      Thread.current[:options] = old_options
    end
  end
end
