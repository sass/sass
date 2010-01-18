require 'rubygems'
require 'rake'

# Note that Haml's gem-compilation process requires access to the filesystem.
# This means that it cannot be automatically run by e.g. GitHub's gem system.
# However, a build server automatically packages the master branch
# every time it's pushed to; this is made available as the haml-edge gem.
HAML_GEMSPEC = Gem::Specification.new do |spec|
  spec.rubyforge_project = 'haml'
  spec.name = File.exist?('EDGE_GEM_VERSION') ? 'haml-edge' : 'haml'
  spec.summary = "An elegant, structured XHTML/XML templating engine.\nComes with Sass, a similar CSS templating engine."
  spec.version = File.read('VERSION').strip
  spec.authors = ['Nathan Weizenbaum', 'Hampton Catlin']
  spec.email = 'haml@googlegroups.com'
  spec.description = <<-END
      Haml (HTML Abstraction Markup Language) is a layer on top of XHTML or XML
      that's designed to express the structure of XHTML or XML documents
      in a non-repetitive, elegant, easy way,
      using indentation rather than closing tags
      and allowing Ruby to be embedded with ease.
      It was originally envisioned as a plugin for Ruby on Rails,
      but it can function as a stand-alone templating engine.
    END

  spec.add_development_dependency 'yard', '>= 0.5.3'
  spec.add_development_dependency 'maruku', '>= 0.5.9'

  # We need the revision file to exist,
  # so we just create it if it doesn't.
  # It'll usually just get overwritten, though.
  File.open('REVISION', 'w') { |f| f.puts "(unknown)" } unless File.exist?('REVISION')
  readmes = FileList.new('*') do |list|
    list.exclude(/(^|[^.a-z])[a-z]+/)
    list.exclude('TODO')
    list.include('REVISION')
  end.to_a
  spec.executables = ['haml', 'html2haml', 'sass', 'css2sass']
  spec.files = FileList['rails/init.rb', 'lib/**/*', 'bin/*', 'test/**/*',
    'extra/**/*', 'Rakefile', 'init.rb', '.yardopts'].to_a + readmes
  spec.homepage = 'http://haml-lang.com/'
  spec.has_rdoc = true
  spec.extra_rdoc_files = readmes
  spec.rdoc_options += [
    '--title', 'Haml',
    '--main', 'README.rdoc',
    '--exclude', 'lib/haml/buffer.rb',
    '--line-numbers',
    '--inline-source'
   ]
  spec.test_files = FileList['test/**/*_test.rb'].to_a
end
