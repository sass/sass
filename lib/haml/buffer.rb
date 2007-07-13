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

    # Gets the current tabulation of the document.
    def tabulation
      @real_tabs + @tabulation
    end

    # Sets the current tabulation of the document.
    def tabulation=(val)
      val = val - @real_tabs
      @tabulation = val > -1 ? val : 0
    end

    # Creates a new buffer.
    def initialize(options = {})
      @options = options
      @quote_escape = options[:attr_wrapper] == '"' ? "&quot;" : "&apos;"
      @other_quote_char = options[:attr_wrapper] == '"' ? "'" : '"'
      @buffer = ""
      @one_liner_pending = false
      @tabulation = 0

      # The number of tabs that Engine thinks we should have
      # @real_tabs + @tabulation is the number of tabs actually output
      @real_tabs = 0
    end

    # Renders +text+ with the proper tabulation. This also deals with
    # making a possible one-line tag one line or not.
    def push_text(text, tab_change = 0, try_one_liner = false)
      if(@tabulation > 0)
        # Have to push every line in by the extra user set tabulation
        text.gsub!(/^/m, '  ' * @tabulation)
      end
      
      @buffer << "#{text}"
      @real_tabs += tab_change
      @one_liner_pending = try_one_liner
    end

    # Properly formats the output of a script that was run in the
    # instance_eval.
    def push_script(result, tabulation, flattened, close_tag = nil)
      if flattened
        result = Haml::Helpers.find_and_preserve(result)
      end
      unless result.nil?
        result = result.to_s
        while result[-1] == ?\n
          # String#chomp is slow
          result = result[0...-1]
        end
        
        if @one_liner_pending && Buffer.one_liner?(result)
          @buffer << result
          @buffer << "</#{close_tag}>\n"
          @one_liner_pending = false
          @real_tabs -= 1
        else
          if @one_liner_pending
            @buffer << "\n"
            tabulation += 1
          end
          
          result = result.gsub(/^/m, tabs(tabulation))
          @buffer << "#{result}\n"
          
          if @one_liner_pending
            @one_liner_pending = false
            @buffer << "#{tabs(tabulation-1)}</#{close_tag}>\n"
          end
        end
      end
      nil
    end

    # Takes the various information about the opening tag for an
    # element, formats it, and adds it to the buffer.
    def open_tag(name, tabulation, atomic, try_one_line, class_id, obj_ref, content, attributes_hash)
      attributes = class_id
      if attributes_hash
        attributes_hash.keys.each { |key| attributes_hash[key.to_s] = attributes_hash.delete(key) }
        self.class.merge_attrs(attributes, attributes_hash)
      end
      self.class.merge_attrs(attributes, parse_object_ref(obj_ref)) if obj_ref

      @one_liner_pending = false
      if atomic
        str = " />\n"
      elsif try_one_line
        @one_liner_pending = true
        str = ">"
      else
        str = ">\n"
      end
      @buffer << "#{tabs(tabulation)}<#{name}#{build_attributes(attributes)}#{str}"
      if content
        if Buffer.one_liner?(content)
          @buffer << "#{content}</#{name}>\n"
        else
          @buffer << "\n#{tabs(@real_tabs+1)}#{content}\n#{tabs(@real_tabs)}</#{name}>\n"
        end
        @one_liner_pending = false
      else
        @real_tabs += 1
      end
    end

    def self.merge_attrs(to, from)
      if to['id'] && from['id']
        to['id'] << '_' << from.delete('id')
      end

      if to['class'] && from['class']
        # Make sure we don't duplicate class names
        from['class'] = (from['class'].split(' ') | to['class'].split(' ')).join(' ')
      end

      to.merge!(from)
    end

    # Some of these methods are exposed as public class methods
    # so they can be re-used in helpers.

    # Takes a hash and builds a list of XHTML attributes from it, returning
    # the result.
    def build_attributes(attributes = {})
      result = attributes.collect do |a,v|
        v = v.to_s
        unless v.nil? || v.empty?
          attr_wrapper = @options[:attr_wrapper]
          if v.include? attr_wrapper
            if v.include? @other_quote_char
              v = v.gsub(attr_wrapper, @quote_escape)
            else
              attr_wrapper = @other_quote_char
            end
          end
          " #{a}=#{attr_wrapper}#{v}#{attr_wrapper}"
        end
      end
      result.sort.join
    end
    
    # Returns whether or not the given value is short enough to be rendered
    # on one line.
    def self.one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end

    private

    @@tab_cache = {}
    # Gets <tt>count</tt> tabs. Mostly for internal use.
    def tabs(count)
      tabs = count + @tabulation
      '  ' * tabs
      @@tab_cache[tabs] ||= '  ' * tabs
    end

    # Takes an array of objects and uses the class and id of the first
    # one to create an attributes hash.
    def parse_object_ref(ref)
      ref = ref[0]
      # Let's make sure the value isn't nil. If it is, return the default Hash.
      return {} if ref.nil?
      class_name = underscore(ref.class)
      id = "#{class_name}_#{ref.id || 'new'}"

      {'id' => id, 'class' => class_name}
    end

    # Changes a word from camel case to underscores.
    # Based on the method of the same name in Rails' Inflector,
    # but copied here so it'll run properly without Rails.
    def underscore(camel_cased_word)
      camel_cased_word.to_s.gsub(/::/, '_').
        gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
        gsub(/([a-z\d])([A-Z])/,'\1_\2').
        tr("-", "_").
        downcase
    end
  end
end

unless String.methods.include? 'old_comp'
  class String # :nodoc
    alias_method :old_comp, :<=>
    
    def <=>(other)
      if other.is_a? NilClass
        -1
      else
        old_comp(other)
      end
    end
  end
    
  class NilClass # :nodoc:
    include Comparable
    
    def <=>(other)
      other.nil? ? 0 : 1
    end
  end
end

