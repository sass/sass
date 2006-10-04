require File.dirname(__FILE__) + '/helpers'

module Haml
  # This is the class where all the parsing and processing of the HAML
  # template is done. It can be directly used by the user by creating a
  # new instance and calling to_html to render the template. For example:
  # 
  #   template = File.load('templates/really_cool_template.haml')
  #   haml_engine = Haml::Engine.new(template)
  #   output = haml_engine.to_html
  #   puts output
  class Engine
    include Haml::Helpers

    # Set the maximum length for a line to be considered a one-liner.
    # Lines <= the maximum will be rendered on one line,
    # i.e. <tt><p>Hello world</p></tt>
    ONE_LINER_LENGTH     = 50
    
    # Keeps track of the ASCII values of the characters that begin a 
    # specially-interpreted line.
    SPECIAL_CHARACTERS   = %w(# . = ~ % /).collect { |c| c[0] }

    # The value of the character that designates that a line is part
    # of a multiline string.
    MULTILINE_CHAR_VALUE = '|'[0]
    
    # Characters that designate that a multiline string may be about
    # to begin.
    MULTILINE_STARTERS   = SPECIAL_CHARACTERS - ["/"[0]]

    # Creates a new instace of Haml::Engine to compile the given
    # template string.
    # 
    # Available options are:
    # 
    # [<tt>scope_object</tt>]  The object within which the template will
    #                          be compiled, via instance_eval. For a Rails
    #                          application, this will typically be an
    #                          instance of ActionView::Base. If not specified,
    #                          this defaults to an instance of the Object class.
    # [<tt>suppress_eval</tt>] Whether or not attribute hashes and Ruby scripts
    #                          designated by <tt>=</tt> or <tt>~</tt> should be
    #                          evaluated. If this is true, said scripts are
    #                          rendered as empty strings. Defaults to false.
    def initialize(template, options = {})
      #turn each of the options into instance variables for the object
      options.each { |k,v| eval("@#{k} = v") }

      @template = template #String
      @result, @precompiled, @to_close_stack = String.new, String.new, []
      @scope_object = Object.new if @scope_object.nil?
    end

    # Processes the template and returns the resulting (X)HTML code as
    # a string.
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
      @to_close_stack.length.times { close_tag }

      # Compile the @precompiled buffer
      compile

      # Return the result string
      @result
    end
    
    private

    # Processes a single line of HAML. <tt>count</tt> does *not* represent the
    # line number; rather, it represents the tabulation count (the number of
    # spaces before the line divided by two).
    # 
    # This method doesn't return anything; it simply processes the line and
    # adds the appropriate code to <tt>@precompiled</tt>.
    def process_line(count, line)
      if line.strip[0, 3] == '!!!'
        push_text %|<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">|
      else
        if count <= @to_close_stack.size && @to_close_stack.size > 0
          (@to_close_stack.size - count).times { close_tag }
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

    # Deals with all the logic of figuring out whether a given line is
    # the beginning, continuation, or end of a multiline sequence. Like
    # process_line, <tt>count</tt> represents the tabulation, not line
    # number.
    # 
    # This returns three values: whether or not the line should be
    # rendered normally, and the original values for line and count (in
    # that order).
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

    # Takes <tt>@precompiled</tt>, a string buffer of Ruby code, and
    # evaluates it in the context of <tt>@scope_object</tt>, after preparing
    # <tt>@scope_object</tt>. The code in <tt>@precompiled</tt> populates
    # <tt>@result</tt> with the compiled XHTML code.
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

    # Puts a line in <tt>@precompiled</tt> that will add the result of <tt>text</tt>
    # (which should be Ruby code or a Ruby string) to <tt>@result</tt> with 
    # appropriate tabulation.
    def push_visible(text)
      @precompiled << "@haml_stack[-1] << #{tabs(@to_close_stack.size).dump} << #{text}\n"
    end

    # Puts a line in <tt>@precompiled</tt> that will evaluate <tt>text</tt> in the
    # context of <tt>@scope_object</tt>, but not output the result.
    def push_silent(text)
      @precompiled << "#{text}\n"
    end

    # Puts a line in <tt>@precompiled</tt> that will add <tt>text</tt> to
    # <tt>@result</tt> with appropriate tabulation without parsing it.
    def push_text(text)
      push_visible("#{text.dump} << \"\\n\"")
    end

    # Puts several lines in <tt>@precompiled</tt> that will cause <tt>text</tt>
    # to be evaluated in the context of <tt>@scope_object</tt> and the result to
    # be formatted and added to <tt>@result</tt>.
    # 
    # If <tt>flattened</tt> is true, Haml::Helpers#find_and_flatten is run on
    # the result before it is added to <tt>@result</tt>
    def push_script(text, flattened = false)
      unless @suppress_eval
        push_silent("haml_temp = #{text}")
        if flattened
          push_silent("haml_temp = find_and_flatten(haml_temp)")
        end
        push_visible("#{wrap_script("haml_temp", @to_close_stack.size)} << \"\\n\"")
      end
    end

    # Takes a hash and builds a list of XHTML attributes from it, returning
    # the result.
    # 
    # *TODO*: have the attributes be evaluated in the context of
    # <tt>@scope_object</tt> in the normal flow of evaluation. As it is, the
    # attribute hashes are evaluated before any other script, so they don't
    # have access to local variables, etc.
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

    # Puts a line in <tt>@precompiled</tt> that will add the opening tag of
    # the element with the given name and attributes.
    def open_tag(name, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)}>"
      @to_close_stack.push name
    end

    # Puts a line in <tt>@precompiled</tt> that will add the closing tag of
    # the most recently opened tag.
    def close_tag
      push_text "</#{@to_close_stack.pop}>"
    end

    # Puts a line in <tt>@precompiled</tt> that will add a tag that doesn't
    # have line breaks before and after the value inside it.
    def one_line_tag(name, value, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)}>#{value}</#{name.to_s}>"
    end

    # Returns whether or not the given value is short enough to be rendered
    # on one line.
    def one_liner?(value)
      value.length <= ONE_LINER_LENGTH && value.scan(/\n/).empty?
    end
    
    # Deals with all the logic of the opening of a tag. Checks whether it
    # needs to be a one-liner, etc.
    # 
    # If <tt>parse</tt> is true, value is parsed as ruby script. If
    # <tt>flattened</tt> is also true, then Haml::Helpers.find_and_flatten
    # is applied to the result.
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

    # Puts a line in <tt>@precompiled</tt> that will create single-line
    # tags with the given name, such as <tt><hello /></tt>.
    def atomic_tag(name, attributes = {})
      push_text "<#{name.to_s}#{build_attributes(attributes)} />"
    end

    # Iterates through the classes and ids supplied through <tt>.</tt>
    # and <tt>#</tt> syntax, and returns a hash with them as attributes,
    # that can then be merged with another attributes hash.
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

    # Parses a line that will render as an XHTML tag, and adds the code that will
    # render that tag to <tt>@precompiled</tt>.
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

    # Renders a line that creates an XHTML tag and has an implicit div because of
    # <tt>.</tt> or <tt>#</tt>.
    def render_div(line)
      render_tag('%div' + line)
    end

    # Renders an XHTML comment.
    def render_comment(line)
      push_text "<!-- #{line[1..line.length].strip} -->"
    end

    # If <tt>@suppress_eval</tt> hasn't been set, evaluates the given
    # string in the context of <tt>@scope_object</tt>.
    def template_eval(args)
       !@suppress_eval ? @scope_object.instance_eval(args) : ""
    end
  end
end
