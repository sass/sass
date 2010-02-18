class CallbacksHandler < YARD::Handlers::Ruby::Legacy::Base
  handles /\Adefine_callback(\s|\()/

  def process
    callback_name = tokval(statement.tokens[2])
    attr_index = statement.comments.each_with_index {|c, i| break i if c[0] == ?@}
    if attr_index.is_a?(Fixnum)
      docstring = statement.comments[0...attr_index]
      attrs = statement.comments[attr_index..-1]
    else
      docstring = statement.comments
      attrs = []
    end

    yieldparams = ""
    attrs.reject! do |a|
      next unless a =~ /^@yield *(\[.*?\])/
      yieldparams = $1
      true
    end

    o = register(MethodObject.new(namespace, "on_#{callback_name}", scope))
    o.docstring = docstring + [
      "@return [void]",
      "@yield #{yieldparams} When the callback is run"
    ] + attrs
    o.signature = true
  end
end
