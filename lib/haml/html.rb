require File.dirname(__FILE__) + '/../haml'

require 'haml/engine'
require 'rubygems'
require 'hpricot'
require 'cgi'

module Haml
  # This class contains the functionality used in the +html2haml+ utility,
  # namely converting HTML documents to Haml templates.
  # It depends on Hpricot for HTML parsing (http://code.whytheluckystiff.net/hpricot/).
  class HTML
    # Creates a new instance of Haml::HTML that will compile the given template,
    # which can either be a string containing HTML or an Hpricot node,
    # to a Haml string when +render+ is called.
    def initialize(template, options = {})
      @@options = options

      if template.is_a? Hpricot::Node
        @template = template
      else
        if template.is_a? IO
          template = template.read
        end

        if @@options[:rhtml]
          match_to_html(template, /<%=(.*?)-?%>/m, 'loud')
          match_to_html(template, /<%(.*?)-?%>/m,  'silent')
        end

        method = @@options[:xhtml] ? Hpricot.method(:XML) : method(:Hpricot)
        @template = method.call(template.gsub('&', '&amp;'))
      end
    end

    # Processes the document and returns the result as a string
    # containing the Haml template.
    def render
      @template.to_haml(0)
    end
    alias_method :to_haml, :render

    module ::Hpricot::Node
      # Returns the Haml representation of the given node,
      # at the given tabulation.
      def to_haml(tabs = 0)
        parse_text(self.to_s, tabs)
      end

      private

      def tabulate(tabs)
        '  ' * tabs
      end

      def parse_text(text, tabs)
        text.strip!
        if text.empty?
          String.new
        else
          lines = text.split("\n")

          lines.map do |line|
            line.strip!
            "#{tabulate(tabs)}#{'\\' if Haml::Engine::SPECIAL_CHARACTERS.include?(line[0])}#{line}\n"
          end.join
        end
      end
    end

    # :stopdoc:

    def self.options
      @@options
    end

    TEXT_REGEXP = /^(\s*).*$/

    class ::Hpricot::Doc
      def to_haml(tabs = 0)
        output = ''
        children.each { |child| output += child.to_haml(0) }
        output
      end
    end

    class ::Hpricot::XMLDecl
      def to_haml(tabs = 0)
        "#{tabulate(tabs)}!!! XML\n"
      end
    end

    class ::Hpricot::DocType
      def to_haml(tabs = 0)
        attrs = public_id.scan(/DTD\s+([^\s]+)\s*([^\s]*)\s*([^\s]*)\s*\/\//)[0]
        if attrs == nil
          raise Exception.new("Invalid doctype")
        end

        type, version, strictness = attrs.map { |a| a.downcase }
        if type == "html"
          version = "1.0"
          strictness = "transitional"
        end

        if version == "1.0" || version.empty?
          version = nil
        end

        if strictness == 'transitional' || strictness.empty?
          strictness = nil
        end

        version = " #{version}" if version
        if strictness
          strictness[0] = strictness[0] - 32
          strictness = " #{strictness}"
        end

        "#{tabulate(tabs)}!!!#{version}#{strictness}\n"
      end
    end

    class ::Hpricot::Comment
      def to_haml(tabs = 0)
        "#{tabulate(tabs)}/\n#{parse_text(self.content, tabs + 1)}"
      end
    end

    class ::Hpricot::Elem
      def to_haml(tabs = 0)
        output = "#{tabulate(tabs)}"
        if HTML.options[:rhtml] && name[0...5] == 'haml:'
          return output + HTML.send("haml_tag_#{name[5..-1]}",
                                    CGI.unescapeHTML(self.innerHTML))
        end

        output += "%#{name}" unless name == 'div' && (attributes.include?('id') || attributes.include?('class'))

        if attributes
          output += "##{attributes['id']}" if attributes['id']
          attributes['class'].split(' ').each { |c| output += ".#{c}" } if attributes['class']
          remove_attribute('id')
          remove_attribute('class')
          output += haml_attributes if attributes.length > 0
        end

        output += "/" if children.length == 0
        output += "\n"

        self.children.each do |child|
          output += child.to_haml(tabs + 1)
        end

        output
      end

      private

      # Returns a string representation of an attributes hash
      # that's prettier than that produced by Hash#inspect
      def haml_attributes
        attrs = attributes.map do |name, value|
          name = name.index(/\W/) ? name.inspect : ":#{name}"
          "#{name} => #{value.inspect}"
        end
        "{ #{attrs.join(', ')} }"
      end
    end

    def self.haml_tag_loud(text)
      "= #{text.gsub(/\n\s*/, ' ').strip}\n"
    end

    def self.haml_tag_silent(text)
      text.split("\n").map { |line| "- #{line.strip}\n" }.join
    end

    private

    def match_to_html(string, regex, tag)
      string.gsub!(regex) do
        "<haml:#{tag}>#{CGI.escapeHTML($1)}</haml:#{tag}>"
      end
    end
    # :startdoc:
  end
end
