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
      @pony_land = HappyMagicPonyLand.new(@base, locals)
      @result = ""
      @to_close_queue = []
      
      #main loop handling line reading
      #and interpretation
      template.each_line do |line|
        count, line = count_levels(line)
        if count <= @to_close_queue.size
          close_tag
        end
        case line.first
          when '.', '#'
            render_div(line)
          when '<'
            render_tag_or_line(line)
          when '/'
            render_comment(line)
          when '='
            add template_eval(line[1, line.length])
          else
            add line
          end
      end
      puts "closing... " + @to_close_queue.inspect
      @to_close_queue.each { close_tag }
      @result
    end
    
    def add(line)
      @result << tabify(@to_close_queue.size) + line + "\n"
    end
    
    def tabify(times)
      @tab_index[times]
    end
    
    def open_tag(name, attributes = {})
     attribute_array = []
     attributes.each do |attr_name, val|
      attribute_array << attr_name.to_s + "='" + val + "'" 
     end
     add "<#{name.to_s}#{attribute_array.empty? ? "" : " "}#{attribute_array.join(" ")}>"
     @to_close_queue.push(name)
    end
    
    def close_tag
      add "</#{@to_close_queue.pop}>"
    end
    
    def render_div(line)
      open_tag("div", parse_attributes(line))
    end
    
    def render_comment(line)
      add "<!-- #{line} -->"
    end
    
    def render_tag_or_line(line)
      line.scan(/[<]([a-zA-Z]+)([a-zA-Z.\#]*)([=]?)([^\n]*)/).each do |tag_name, attributes, action, value|
        puts tag_name
        open_tag(tag_name, parse_attributes(attributes))
        unless value.empty?
          if(action == '=')
            add(value)
          else
            add(template_eval(value))
          end
          close_tag
        end
      end
    end
    
    def template_eval(code)
      @pony_land.instance_eval(code) || ""
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
      [line.index(/[^ ]/), line.strip]
    end
  end
  
  class HappyMagicPonyLand
  
    def initialize(base, attributes = {})
      @_action_view = base
      @table = attributes
    end
    
    def method_missing(name, *args, &block)
      name_as_string = name.to_s
      if name_as_string.last == "="
        @table[name_as_string.scan(/[^=]+/).first.to_sym] = args.first
      elsif @table.has_key? name.to_sym
        @table[name.to_sym]
      elsif @_action_view.respond_to? name
        @_action_view.send(name, *args, &block)
      else 
        nil
      end
    end
  end
end