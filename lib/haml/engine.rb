require File.dirname(__FILE__) + '/helpers'

module Haml #:nodoc:
  class Engine
    include Haml::Helpers
  
    # Set the maximum length for a line to be considered a one-liner
    # Lines <= the maximum will be rendered on one line,
    # i.e. <tt><p>Hello world</p></tt>
    ONE_LINER_LENGTH = 50
    MULTILINE_CHAR_VALUE = '|'[0]
  
    def initialize(view)
      @view = view
      @result = String.new
      @to_close_queue = []
    end

    def render(template, local_assigns={})
      assigns = @view.assigns.dup
  
      # Do content for layout on its own to keep things working in partials
      if content_for_layout = @view.instance_variable_get("@content_for_layout")
        assigns['content_for_layout'] = content_for_layout
      end

      # Get inside the view object's world
      @view.instance_eval do
        # Set all the instance variables
        assigns.each do |key,val|
          instance_variable_set "@#{key}", val
        end
        # Set all the local assigns
        local_assigns.each do |key,val|
          class << self; self; end.send(:define_method, key) { val }
        end
      end

      # Process each line of the template returning the resulting string
      template.each_with_index do |line, index|
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
        
        case line.first
        when '.', '#'
          render_div(line)
        when '%'
          render_tag(line)
        when '/'
          render_comment(line)
        when '='
          add template_eval(line[1, line.length]).to_s
        when '~'
          add find_and_flatten(template_eval(line[1, line.length])).to_s
        else
          add line.strip
        end
      end
      return count, line
    end

    def handle_multiline(count, line)
      # The code to handle how a multi-line object should work.
      if @multiline_buffer && line[-1] == MULTILINE_CHAR_VALUE # '|' is 124 
        # A multiline string is active, and is being continued 
        @multiline_buffer += line[0...-1]
        supress_render = true
      elsif line[-1] == MULTILINE_CHAR_VALUE 
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

    def open_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>"
      @to_close_queue.push name
    end

    def one_line_tag(name, value, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>#{value}</#{name.to_s}>"
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

    def build_attributes(attributes = {})
      attributes.empty? ? String.new : String.new(' ') << (attributes.collect {|a,v| "#{a.to_s}='#{v.to_s}'" unless v.nil? }).compact.join(' ')
    end

    def close_tag
      add "</#{@to_close_queue.pop}>"
    end

    def render_div(line)
      render_tag('%div' + line)
    end

    def render_comment(line)
      add "<!-- #{line[1..line.length].strip} -->"
    end

    def render_tag(line)
      line.scan(/[%]([-_a-z1-9]+)([-_a-z\.\#]*)(\{.*\})?(\[.*\])?([=\/\~]?)?(.*)?/).each do |tag_name, attributes, attributes_hash, object_ref, action, value|
        attributes = parse_class_and_id(attributes.to_s)
        attributes.merge!(template_eval(attributes_hash)) unless (attributes_hash.nil? || attributes_hash.empty?)

        if object_ref && (object_ref = template_eval(object_ref).first)
          class_name = object_ref.class.to_s.underscore
          attributes.merge!(:id => "#{class_name}_#{object_ref.id}", :class => class_name)
        end

        if action == '/'
          atomic_tag(tag_name, attributes)
        elsif action == '=' || action == '~'
          value = template_eval(value)
          value = find_and_flatten(value) if action == '~'
          print_tag(tag_name, value.to_s, attributes) if value
        else
          print_tag(tag_name, value.to_s.strip, attributes)
        end
      end
    end

    # Searches for `#` and `.` characters indicating id and class attributes
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

    def one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end

    # Evaluates input in the context of the current ActionView instance
    def template_eval(args)
      @view.instance_eval(args)
    end
  end
end
