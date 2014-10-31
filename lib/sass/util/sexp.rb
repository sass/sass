module Sass::Util::Sexp
  protected

  def sass(*names)
    sexp = s(:const, :Sass)
    names.each {|n| sexp = s(:colon2, sexp, n)}
    sexp
  end

  def resbody(type, var, *body)
    body = s(nil) if body.empty?
    s(:resbody, s(:array, type, s(:lasgn, var, s(:gvar, :$!))), *body)
  end

  def each(enum, var, *body)
    s(:for, enum, s(:lasgn, var), s(:block, *body))
  end

  def chain(obj, *methods)
    sexp = obj
    methods.each {|m| sexp = s(:call, sexp, m)}
    sexp
  end

  def sass_error(arg, options = nil)
    call = s(:call, sass(:SyntaxError), :new, arg)
    call << lit(options) if options
    s(:call, nil, :raise, call)
  end

  def lit(obj)
    case obj
    when true; s(:true)
    when false; s(:false)
    when nil; s(:nil)
    when Symbol, Numeric; s(:lit, obj)
    when String; s(:str, obj)
    when Array; s(:array, *obj.map {|e| lit(e)})
    when Hash; s(:hash, *Sass::Util.flatten(obj.map {|(k, v)| [lit(k), lit(v)]}, 1))
    else; raise "Can't create a literal for #{obj}."
    end
  end

  def or_asgn(var, value)
    s(:op_asgn_or, s(:lvar, var), s(:lasgn, var, value))
  end

  def var(var)
    var.to_s[0] == ?@ ? s(:ivar, var) : s(:lvar, var)
  end

  def asgn(var, value)
    var.to_s[0] == ?@ ? s(:iasgn, var, value) : s(:lasgn, var, value)
  end

  def to_string(value, quote = nil)
    call = s(:call, value, :to_s)
    return call unless quote
    call << lit(:quote => quote)
  end
end
