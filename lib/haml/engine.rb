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

    def initialize(template, action_view=nil)
      @view = action_view
      @template = template #String
      @result = String.new
      @to_close_queue = []
    end
    
    def to_html
      # Process each line of the template
      @template.each_with_index do |line, index|
        count, line = count_soft_tabs(line)
        surpress_render, line, count = handle_multiline(count, line)

        if !surpress_render && count && line
          count, line = process_line(count, line)
        end
      end
  
      # Close all the open tags
      @to_close_queue.length.times { close_tag }
  
      # Return the result string
      @result
    end

    def process_line(count, line)
      if line.strip[0, 3] == '!!!'
        @result << %|<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n|
      else
        if count <= @to_close_queue.size && @to_close_queue.size > 0
          (@to_close_queue.size - count).times { close_tag }
        end

        case line[0..0]
        when '.', '#'
          render_div(line)
        when '%'
          render_tag(line)
        when '/'
          render_comment(line)
        when '='
          add template_eval(line[1, line.length]).to_s if @view
        when '~'
          add find_and_flatten(template_eval(line[1, line.length])).to_s if @view
        else
          add line.strip
        end
      end
      return count, line
    end

    def handle_multiline(count, line)
      # Multilines are denoting by ending with a `|` (124)
      if (line[-1] == MULTILINE_CHAR_VALUE) && @multiline_buffer
        # A multiline string is active, and is being continued 
        @multiline_buffer += line[0...-1]
        supress_render = true
      elsif (line[-1] == MULTILINE_CHAR_VALUE) && (MULTILINE_STARTERS.include? line[0])
        # A multiline string has just been activated, start adding the lines
        @multiline_buffer = line[0...-1]
        @multiline_count = count
        supress_render = true
      elsif @multiline_buffer
        # A multiline string has just ended, make line into the result
        process_line(@multiline_count, @multiline_buffer)
        @multiline_buffer = nil
        supress_render = false
      end
      
      return supress_render, line, count
    end

    def add(line)
      return if line.nil?
      line.to_s.each_line do |me| 
        @result << tabs(@to_close_queue.size) << me.chomp << "\n"
      end
    end

    def build_attributes(attributes = {})
      attributes.empty? ? String.new : String.new(' ') << (attributes.collect {|a,v| "#{a.to_s}='#{v.to_s}'" unless v.nil? }).compact.join(' ')
    end

    def open_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>"
      @to_close_queue.push name
    end

    def close_tag
      add "</#{@to_close_queue.pop}>"
    end

    def one_line_tag(name, value, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>#{value}</#{name.to_s}>"
    end

    def one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end

    def print_tag(name, value, attributes = {})
      unless value.empty?
        if one_liner? value
          one_line_tag(name, value, attributes)
        else
          open_tag(name, attributes)
          add value
          close_tag
        end
      else
        open_tag(name, attributes)
        add value
      end
    end

    # Creates single line tags, i.e. <tt><hello /></tt>
    def atomic_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)} />"
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
        if @view
          if object_ref && (object_ref = template_eval(object_ref).first)
            class_name = object_ref.class.to_s.underscore
            attributes.merge!(:id => "#{class_name}_#{object_ref.id}", :class => class_name)
          end
        end

        unless (attributes_hash.nil? || attributes_hash.empty?) 
          # Determine whether to eval the attributes hash in the context of a template
          add_attributes = @view ? template_eval(attributes_hash) : eval(attributes_hash)
          attributes.merge!(add_attributes)
        end

        case action
        when '/'
          atomic_tag(tag_name, attributes)
        when '=', '~'
          value = template_eval(value) if @view
          value = find_and_flatten(value) if action == '~' and @view
          print_tag(tag_name, value.to_s, attributes) if value
        else
          print_tag(tag_name, value.to_s.strip, attributes)
        end
      end
    end

    def render_div(line)
      render_tag('%div' + line)
    end

    def render_comment(line)
      add "<!-- #{line[1..line.length].strip} -->"
    end
    
    def template_eval(args)
      @view.instance_eval(args)
    end
  end
end
