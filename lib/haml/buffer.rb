module Haml
  # This class is used only internally. It holds the buffer of XHTML that
  # is eventually output by Haml::Engine's to_html method. It's called
  # from within the precompiled code, and helps reduce the amount of
  # processing done within instance_eval'd code.
  class Buffer
    include Haml::Helpers
    
    # Set the maximum length for a line to be considered a one-liner.
    # Lines <= the maximum will be rendered on one line,
    # i.e. <tt><p>Hello world</p></tt>
    ONE_LINER_LENGTH     = 50
    
    # The string that holds the compiled XHTML. This is aliased as
    # _erbout for compatibility with ERB-specific code.
    attr_accessor :buffer
    
    # Creates a new buffer.
    def initialize(options = {})
      @options = options
      @quote_escape = options[:attr_wrapper] == '"' ? "&quot;" : "&apos;"
      @buffer = ""
      @one_liner_pending = false
    end
    
    # Renders +text+ with the proper tabulation. This also deals with
    # making a possible one-line tag one line or not.
    def push_text(text, tabulation)
      if @one_liner_pending && one_liner?(text)
        @buffer << text
      else
        if @one_liner_pending
          @buffer << "\n"
          @one_liner_pending = false
        end
        @buffer << "#{tabs(tabulation)}#{text}\n"
      end
    end
    
    # Properly formats the output of a script that was run in the
    # instance_eval.
    def push_script(result, tabulation, flattened)
      if flattened
        result = find_and_flatten(result)
      end
      unless result.nil?
        result = result.to_s.chomp.gsub("\n", "\n#{tabs(tabulation)}")
        push_text result, tabulation
      end
      nil
    end
    
    # Takes the various information about the opening tag for an
    # element, formats it, and adds it to the buffer.
    def open_tag(name, tabulation, atomic, try_one_line, class_id, attributes_hash, obj_ref)
      attributes = {}
      attributes.merge!(parse_object_ref(obj_ref)) if obj_ref
      attributes.merge!(parse_class_and_id(class_id)) if class_id
      attributes.merge!(attributes_hash) unless attributes_hash.nil? || attributes_hash.empty?
      
      @buffer << "#{tabs(tabulation)}<#{name}#{build_attributes(attributes)}"
      @one_liner_pending = false
      if atomic
        @buffer << " />\n"
      else
        if try_one_line
          @one_liner_pending = true
          @buffer << ">"
        else
          @buffer << ">\n"
        end
      end
    end
    
    # Creates a closing tag with the given name.
    def close_tag(name, tabulation)
      if @one_liner_pending
        @buffer << "</#{name}>\n"
        @one_liner_pending = false
      else
        push_text("</#{name}>", tabulation)
      end
    end
    
    # Opens an XHTML comment.
    def open_comment(try_one_line, conditional, tabulation)
      conditional << ">" if conditional
      @buffer << "#{tabs(tabulation)}<!--#{conditional.to_s} "
      if try_one_line
        @one_liner_pending = true
      else
        @buffer << "\n"
      end
    end
    
    # Closes an XHTML comment.
    def close_comment(has_conditional, tabulation)
      close_tag = has_conditional ? "<![endif]-->" : "-->"
      if @one_liner_pending
        @buffer << " #{close_tag}\n"
        @one_liner_pending = false
      else
        push_text(close_tag, tabulation)
      end
    end
    
    private
    
    # Gets <tt>count</tt> tabs. Mostly for internal use.
    def tabs(count)
      '  ' * count
    end
    
    # Iterates through the classes and ids supplied through <tt>.</tt>
    # and <tt>#</tt> syntax, and returns a hash with them as attributes,
    # that can then be merged with another attributes hash.
    def parse_class_and_id(list)
      attributes = {}
      list.scan(/([#.])([-_a-zA-Z0-9]+)/) do |type, property|
        case type
        when '.'
          if attributes[:class]
            attributes[:class] += " "
          else
            attributes[:class] = ""
          end
          attributes[:class] += property
        when '#'
          attributes[:id] = property
        end
      end
      attributes
    end
    
    # Takes an array of objects and uses the class and id of the first
    # one to create an attributes hash.
    def parse_object_ref(ref)
      ref = ref[0]
      return {} if ref.nil?
      class_name = ref.class.to_s.underscore
      {:id => "#{class_name}_#{ref.id}", :class => class_name}
    end
    
    # Takes a hash and builds a list of XHTML attributes from it, returning
    # the result.
    def build_attributes(attributes = {})
      attributes.each do |key, value|
        unless key.is_a? String
          attributes.delete key
          attributes[key.to_s] = value
        end
      end
      result = attributes.sort.collect do |a,v|
        unless v.nil?
          v = v.to_s
          attr_wrapper = @options[:attr_wrapper]
          if v.include? attr_wrapper
            v = v.gsub(attr_wrapper, @quote_escape)
          end
          "#{a.to_s}=#{attr_wrapper}#{v}#{attr_wrapper}"
        end
      end
      result = result.compact.join(' ')
      (attributes.empty? ? String.new : String.new(' ')) + result
    end
    
    # Returns whether or not the given value is short enough to be rendered
    # on one line.
    def one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end
  end
end
