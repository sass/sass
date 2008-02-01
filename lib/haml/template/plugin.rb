# :stopdoc:
# This file makes Haml work with Rails
# using the > 2.0.1 template handler API.

module Haml
  class Template
    include ActionView::TemplateHandlers::Compilable if defined?(ActionView::TemplateHandlers::Compilable)

    def self.line_offset
      1
    end

    def compilable?
      true
    end
 
    def line_offset
      self.class.line_offset
    end

    def initialize(view)
      @view = view
    end

    def compile(template)
      options = Haml::Template.options.dup
      Haml::Engine.new(template, options).send(:precompiled_with_ambles, [])
    end
   
    def cache_fragment(block, name = {}, options = nil)
      @view.fragment_for(block, name, options) do
        eval("_hamlout.buffer", block.binding)
      end
    end
   
    def read_template_file(template_path, extension)
      File.read(template_path)
    end
  end
end

ActionView::Base.register_template_handler(:haml, Haml::Template)
# :startdoc:
