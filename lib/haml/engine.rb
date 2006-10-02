require File.dirname(__FILE__) + '/helpers'

module Haml #:nodoc:
  class Engine
    include Haml::Helpers

    # Set the maximum length for a line to be considered a one-liner
    # Lines <= the maximum will be rendered on one line,
    # i.e. <tt><p>Hello world</p></tt>
    ONE_LINER_LENGTH     = 50
    SPECIAL_CHARACTERS   = %w(# . = ~ % /).collect { |c| c[0] }
    MULTILINE_CHAR_VALUE = '|'[0]
    MULTILINE_STARTERS   = SPECIAL_CHARACTERS - ["/"[0]]

    def initialize(template, options = {})
      #turn each of the options into instance variables for the object
      options.each { |k,v| eval("@#{k} = v") }

      @template = template #String
      @result, @precompiled, @to_close_queue = String.new, String.new, []
      @scope_object = Object.new if @scope_object.nil?
    end

    def to_html
      # Process each line of the template
      @template.each_with_index do |line, index|
        count, line = count_soft_tabs(line)
        suppress_render, line, count = handle_multiline(count, line)

        if !suppress_render && count && line
          count, line = process_line(count, line)
        end
      end

      # Make sure an ending multiline gets closed
      handle_multiline(0, nil)

      # Close all the open tags
      @to_close_queue.length.times { close_tag }

      # Compile the @precompiled buffer
      compile

      # Return the result string
      @result
    end

    def process_line(count, line)
      if line.strip[0, 3] == '!!!'
        push_text %|<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">|
      else
        if count <= @to_close_queue.size && @to_close_queue.size > 0
          (@to_close_queue.size - count).times { close_tag }
        end
        if line.length > 0
          case line[0].chr
          when '.', '#'
            render_div(line)
          when '%'
            render_tag(line)
          when '/'
            render_comment(line)
          when '='
            push_script(line[1, line.length])
          when '~'
            push_script(line[1, line.length], true)
          else
            push_text line.strip
        end
        end
      end
    end

    def handle_multiline(count, line)
      # Multilines are denoting by ending with a `|` (124)
      if line && (line[-1] == MULTILINE_CHAR_VALUE) && @multiline_buffer
        # A multiline string is active, and is being continued 
        @multiline_buffer += line[0...-1]
        suppress_render = true
      elsif line && (line[-1] == MULTILINE_CHAR_VALUE) && (MULTILINE_STARTERS.include? line[0])
        # A multiline string has just been activated, start adding the lines
        @multiline_buffer = line[0...-1]
        @multiline_count = count
        suppress_render = true
      elsif @multiline_buffer
        # A multiline string has just ended, make line into the result
        process_line(@multiline_count, @multiline_buffer)
        @multiline_buffer = nil
        suppress_render = false
      end

      return suppress_render, line, count
    end

    def compile
      # Set the local variables pointing to the buffer
      result = @result
      @scope_object.instance_eval do
        @haml_stack ||= Array.new
        @haml_stack.push(result)
        self.class.instance_eval { include Haml::Helpers }
      end
      
      # Evaluate the buffer in the context of the scope object
      # This automatically dumps the result into @result
      @scope_object.instance_eval @precompiled
      
      # Get rid of the current buffer
      @scope_object.instance_eval do
        @haml_stack.pop
      end      
    end

    def push_visible(text)
      @precompiled << "@haml_stack[-1] << #{tabs(@to_close_queue.size).dump} << #{text}\n"
    end

    def push_silent(text)
      @precompiled << "#{text}\n"
    end

    def push_text(text)
      push_visible("#{text.dump} << \"\\n\"")
    end

    def push_script(text, flattened = false)
      unless @suppress_eval
        push_silent("haml_temp = #{text}")
        if flattened
          push_silent("haml_temp = find_and_flatten(haml_temp)")
        end
        push_visible("#{wrap_script("haml_temp", @to_close_queue.size)} << \"\\n\"")
      end
    end

    def build_attributes(attributes = {})
      result = attributes.collect do |a,v|
        unless v.nil?
          first_quote_type = v.to_s.scan(/['"]/).first
          quote_type = (first_quote_type == "'") ? '"' : "'"
          "#{a.to_s}=#{quote_type}#{v.to_s}#{quote_type}"
        end
      end
      result = result.compact.join(' ')
      (attributes.empty? ? String.new : String.new(' ')) + result
    end

    def open_tag(name, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)}>"
      @to_close_queue.push name
    end

    def close_tag
      push_text "</#{@to_close_queue.pop}>"
    end

    def one_line_tag(name, value, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)}>#{value}</#{name.to_s}>"
    end

    def one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end
    
    def push_tag(name, value, attributes = {}, parse = false, flattened = false)
      unless value.empty?
        if !parse && one_liner?(value)
          one_line_tag(name, value, attributes)
        else
          open_tag(name, attributes)
          if parse
            push_script(value, flattened)
          else
            push_text(value)
          end
          close_tag
        end
      else
        open_tag(name, attributes)
      end
    end

    # Creates single line tags, i.e. <tt><hello /></tt>
    def atomic_tag(name, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)} />"
    end

    def parse_class_and_id(list)
      attributes = {}
      list.scan(/([#.])([-a-zA-Z_()]+)/).each do |type, property|
        case type
        when '.'
          attributes[:class] = property
        when '#'
          attributes[:id] = property
        end
      end
      attributes
    end

    def render_tag(line)
      line.scan(/[%]([-_a-z1-9]+)([-_a-z\.\#]*)(\{.*\})?(\[.*\])?([=\/\~]?)?(.*)?/).each do |tag_name, attributes, attributes_hash, object_ref, action, value|
        attributes = parse_class_and_id(attributes.to_s)

        #SimplyHelpful style logic with the [@model] helper
        if object_ref && (object_ref = template_eval(object_ref).first)
          class_name = object_ref.class.to_s.underscore
          attributes.merge!(:id => "#{class_name}_#{object_ref.id}", :class => class_name)
        end

        unless (attributes_hash.nil? || attributes_hash.empty?) 
          # Determine whether to eval the attributes hash in the context of a template
          add_attributes = template_eval(attributes_hash)
          attributes.merge!(add_attributes)
        end

        case action
        when '/'
          atomic_tag(tag_name, attributes)
        when '=', '~'
          flattened = (action == '~')
          push_tag(tag_name, value.to_s, attributes, true, flattened) if value
        else
          push_tag(tag_name, value.to_s.strip, attributes)
        end
      end
    end

    def render_div(line)
      render_tag('%div' + line)
    end

    def render_comment(line)
      push_text "<!-- #{line[1..line.length].strip} -->"
    end

    def template_eval(args)
       !@suppress_eval ? @scope_object.instance_eval(args) : ""
    end
  end
end
