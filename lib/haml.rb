module HAML

  class TemplateEngine
    
    def initialize(base)
      @base = base
      @tab_index = ["", "  "]
      #pre-build the tab index up to 9
      10.times do |num|
        @tab_index << @tab_index.last + "  " 
      end
    end
    
    def render(template = "", locals = {})
      @result = ""
      @to_close_queue = []

      locals.each do |variable_name, value|
        @base.instance_eval "@" + variable_name.to_s + " = value"
      end
      
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

    #used to create single line tags... aka <hello />
    def atomic_tag(name, attributes = {})
      add "<#{name.to_s}#{build_attributes(attributes)} />"
    end

    def build_attributes(attributes = {})
      return "" if attributes.empty?
      " " + (attributes.collect { |attr_name, val| attr_name.to_s + "='" + val + "'" }).join(" ")
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
      line.scan(/[%]([-_a-z1-9]+)([-_a-z\.\#]*)([=\/]?)([^\n]*)/).each do |tag_name, attributes, action, value|
        val = template_eval(value)
        attribute_hash = parse_attributes(attributes)
        if val.class == Hash
          attribute_hash.merge!(val) && val = nil
        elsif !val.nil?
          val = val.to_s
        end

        #check to see if we're a one liner
        if(action == "\/")
          atomic_tag(tag_name, attribute_hash)
        else
          if(!val.nil? && ((val.length < 50) || val.scan(/\n/).empty?))
            one_line_tag(tag_name, val, attribute_hash)
          else
            open_tag(tag_name, attribute_hash)
            
            if(action == '=')
              add(val)
              close_tag
            end
          end
        end
      end
    end
    
    def template_eval(code)
      @base.instance_eval(code)
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
  end
  
end