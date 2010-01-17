class InheritedHashHandler < YARD::Handlers::Ruby::Legacy::Base
  handles /\Ainherited_hash(\s|\()/

  def process
    hash_name = tokval(statement.tokens[2])
    name = statement.comments.first.strip
    type = statement.comments[1].strip

    o = register(MethodObject.new(namespace, hash_name, scope))
    o.docstring = [
      "Gets a #{name} from this {Environment} or one of its \\{#parent}s.",
      "@param name [String] The name of the #{name}",
      "@return [#{type}] The #{name} value",
    ]
    o.signature = true
    o.parameters = ["name"]

    o = register(MethodObject.new(namespace, "set_#{hash_name}", scope))
    o.docstring = [
      "Sets a #{name} in this {Environment} or one of its \\{#parent}s.",
      "If the #{name} is already defined in some environment,",
      "that one is set; otherwise, a new one is created in this environment.",
      "@param name [String] The name of the #{name}",
      "@param value [#{type}] The value of the #{name}",
      "@return [#{type}] `value`",
    ]
    o.signature = true
    o.parameters = ["name", "value"]

    o = register(MethodObject.new(namespace, "set_local_#{hash_name}", scope))
    o.docstring = [
      "Sets a #{name} in this {Environment}.",
      "Ignores any parent environments.",
      "@param name [String] The name of the #{name}",
      "@param value [#{type}] The value of the #{name}",
      "@return [#{type}] `value`",
    ]
    o.signature = true
    o.parameters = ["name", "value"]
  end
end
