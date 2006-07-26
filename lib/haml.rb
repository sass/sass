module HAML

  class TemplateEngine
    
    def initialize(base)
      @base = base
      @tab_index = ["", "  "]
      @happy_land = HappyLand.new(@base, @base.assigns)
      #pre-build the tab index up to 9
      20.times do |num|
        @tab_index << @tab_index.last + "  " 
      end
    end
    
    def render(template = "", locals = {})
      @result = ""
      @to_close_queue = []

      #this helps get the right values for helpers.
      @base.assigns.each do |key,value|
        @base.instance_eval("@#{key} = value")
      end
      
      @happy_land.set_locals(locals)
      
      #breakpoint
      
      #main loop handling line reading
      #and interpretation
      template.each_line do |line|
        if line.strip[0, 3] == "!!!"
          @result << %|<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">\n|
        else
          count, line = count_levels(line)
          #puts count.to_s + "::" + line
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
              add template_eval(line[1, line.length])
            else
              add line
            end
        end
      end
      
      @to_close_queue.length.times { close_tag }
      @result
    end

    def add(line)
      return nil if line.nil?
      line.each_line { |me| add_single(me) }
    end
    
    def add_single(line = "")
      @result << @tab_index[@to_close_queue.size]
      @result << line.chomp + "\n"
    end
    
    def open_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>"
      @to_close_queue.push(name)
    end

    def one_line_tag(name, value, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)}>#{value}</#{name.to_s}>"
    end
    
    def print_tag(name, value, attributes = {})
      unless value.empty?
        if one_liner?(value)
          one_line_tag(name, value, attributes)
        else
          open_tag(name, attributes)
          add(value)
          close_tag
        end
      else
        open_tag(name, attributes)
        add(value)
      end
    end

    #used to create single line tags... aka <hello />
    def atomic_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)} />"
    end

    def build_attributes(attributes = {})
      return "" if attributes.empty?
      " " + (attributes.collect { |attr_name, val| attr_name.to_s + "='" + val.to_s + "'" }).join(" ")
    end
    
    def close_tag
      add "</#{name = @to_close_queue.pop}>"
    end
    
    def render_div(line)
      render_tag("%div" + line)
    end
    
    def render_comment(line)
      add "<!-- #{line} -->"
    end
    
    def render_tag(line)
      broken_up = line.scan(/[%]([-_a-z1-9]+)([-_a-z\.\#]*)(\{.*\})?([=\/]?)?([^\n]*)?/)
      broken_up.each do |tag_name, attributes, attributes_hash, action, value|
        attributes = parse_attributes(attributes.to_s)
        
        unless(attributes_hash.nil? || attributes_hash.empty?)
          attributes_hash = template_eval(attributes_hash)
          attributes = attributes.merge(attributes_hash)
        end
        
        #check to see if we're a one liner
        if(action == "\/")
          atomic_tag(tag_name, attributes)
        elsif(action == "=")
          value = template_eval(value)
          print_tag(tag_name, value.to_s, attributes) if value != false
        else
          print_tag(tag_name, value, attributes)
        end
      end
    end
    

    def parse_attributes(list)
      attributes = {}
      list.scan(/([#.])([-a-zA-Z_()]+)/).each do |type, property|
       case type
         when "."
           attributes[:class] = property
         when "#"
           attributes[:id] = property
       end
      end
      attributes
    end
    
    def count_levels(line)
      [line.index(/[^ ]/)/2, line.strip]
    end
    
    def one_liner?(value)
      ((value.length < 50) && value.scan(/\n/).empty?)
    end
    
    def template_eval(code)
      #@base.instance_eval(code)
      #render :inline => "<%=#{code}%>"
      @happy_land.instance_eval(code)
    end
    
  end
  
  class HappyLand #:nodoc
    def initialize(base, hash_of_assigns, hash_of_locals = {})
      base.instance_variables.each do |key|
        value = base.instance_eval(key)
        eval("#{key} = value")
      end
      hash_of_assigns.each do |key, value|
        eval("@#{key} = value")
      end
      @__locals = hash_of_locals
      @__base = base
    end
    
    def set_locals(hash_of_locals)
      @__locals.merge!(hash_of_locals)
    end
    
    def instance_eval(code)
      eval(code)
    end
    
    def method_missing(action, *args, &block)
      if action.to_s.first == "@"
        @__base.instance_eval(action)
      else
        @__locals[action.to_s] || @__locals[action.to_sym] || @__base.send(action, *args, &block)
      end
    end
  end
  
end