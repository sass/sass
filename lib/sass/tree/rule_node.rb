module Sass::Tree
  class RuleNode < Node
    # The character used to include the parent selector
    PARENT = '&'

    attr_accessor :rules

    def initialize(rule, options)
      @rules = [rule]
      super(options)
    end

    def rule
      rules.first
    end

    def rule=(rule)
      self.rules = [rule]
    end

    def ==(other)
      self.class == other.class && rules == other.rules && super
    end

    def add_rules(node)
      @rules += node.rules
    end

    def continued?
      @rules.last[-1] == ?,
    end

    def to_s(tabs, super_rules = nil)
      resolve_parent_refs!(super_rules)

      attributes = []
      sub_rules = []

      rule_separator = @style == :compressed ? ',' : ', '
      line_separator = [:nested, :expanded].include?(@style) ? ",\n" : rule_separator
      rule_indent = '  ' * (tabs - 1)
      per_rule_indent, total_indent = [:nested, :expanded].include?(@style) ? [rule_indent, ''] : ['', rule_indent]

      total_rule = total_indent + @rules.map do |line|
        per_rule_indent + line.join(rule_separator)
      end.join(line_separator)

      children.each do |child|
        if child.is_a? RuleNode
          sub_rules << child
        else
          attributes << child
        end
      end

      to_return = ''
      if !attributes.empty?
        old_spaces = '  ' * (tabs - 1)
        spaces = '  ' * tabs
        if @options[:line_comments] && @style != :compressed
          to_return << "#{old_spaces}/* line #{line}"

          if filename
            relative_filename = if @options[:css_filename]
              begin
                Pathname.new(filename).relative_path_from(  
                  Pathname.new(File.dirname(@options[:css_filename]))).to_s
              rescue ArgumentError
                nil
              end
            end
            relative_filename ||= filename
            to_return << ", #{relative_filename}"
          end

          to_return << " */\n"
        end

        if @style == :compact
          attributes = attributes.map { |a| a.to_s(1) }.select{|a| a && a.length > 0}.join(' ')
          to_return << "#{total_rule} { #{attributes} }\n"
        elsif @style == :compressed
          attributes = attributes.map { |a| a.to_s(1) }.select{|a| a && a.length > 0}.join(';')
          to_return << "#{total_rule}{#{attributes}}"
        else
          attributes = attributes.map { |a| a.to_s(tabs + 1) }.select{|a| a && a.length > 0}.join("\n")
          end_attrs = (@style == :expanded ? "\n" + old_spaces : ' ')
          to_return << "#{total_rule} {\n#{attributes}#{end_attrs}}\n"
        end
      end

      tabs += 1 unless attributes.empty? || @style != :nested
      sub_rules.each do |sub|
        to_return << sub.to_s(tabs, @rules)
      end

      to_return
    end

    protected

    def resolve_parent_refs!(super_rules)
      if super_rules.nil?
        @rules.each do |line|
          line.map! do |rule|
            if rule.include?(:parent)
              raise Sass::SyntaxError.new("Base-level rules cannot contain the parent-selector-referencing character '#{PARENT}'.", self.line)
            end

            rule.join
          end.compact!
        end
        return
      end

      new_rules = []
      super_rules.each do |super_line|
        @rules.each do |line|
          new_rules << []

          super_line.each do |super_rule|
            line.each do |rule|
              rule.unshift(:parent, " ") unless rule.include?(:parent)

              new_rules.last << rule.map do |segment|
                next segment unless segment == :parent
                super_rule
              end.join
            end
          end
        end
      end
      @rules = new_rules
    end

    def perform!(environment)
      @rules = @rules.map {|r| parse_selector(interpolate(r, environment))}
      super
    end

    def parse_selector(text)
      scanner = StringScanner.new(text)
      rules = [[]]

      while scanner.rest?
        rules.last << scanner.scan(/[^",&]*/)
        case scanner.scan(/./)
        when '&'; rules.last << :parent
        when ','
          scanner.scan(/\s*/)
          rules << [] if scanner.rest?
        when '"'
          rules.last << '"' << scanner.scan(/([^"\\]|\\.)*/)
          # We don't want to enforce that strings are closed,
          # but we do want to consume quotes or trailing backslashes.
          rules.last << scanner.scan(/./) if scanner.rest?
        end
      end

      rules.map! do |l|
        Haml::Util.merge_adjacent_strings(l).reject {|r| r.is_a?(String) && r.empty?}
      end

      rules
    end
  end
end
