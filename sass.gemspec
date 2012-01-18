require 'rubygems'

# Note that Sass's gem-compilation process requires access to the filesystem.
# This means that it cannot be automatically run by e.g. GitHub's gem system.
# However, a build server automatically packages the master branch
# every time it's pushed to; this is made available as a prerelease gem.
SASS_GEMSPEC = Gem::Specification.new do |spec|
  spec.rubyforge_project = 'sass'
  spec.name = 'sass'
  spec.summary = "A powerful but elegant CSS compiler that makes CSS fun again."
  spec.version = File.read(File.dirname(__FILE__) + '/VERSION').strip
  spec.authors = ['Nathan Weizenbaum', 'Chris Eppstein', 'Hampton Catlin']
  spec.email = 'sass-lang@googlegroups.com'
  spec.description = <<-END
      Sass makes CSS fun again. Sass is an extension of CSS3, adding
      nested rules, variables, mixins, selector inheritance, and more.
      It's translated to well-formatted, standard CSS using the
      command line tool or a web-framework plugin.
    END

  spec.required_ruby_version = '>= 1.8.7'
  spec.add_development_dependency 'yard', '>= 0.5.3'
  spec.add_development_dependency 'maruku', '>= 0.5.9'

  readmes = Dir['*'].reject{ |x| x =~ /(^|[^.a-z])[a-z]+/ || x == "TODO" }
  spec.executables = ['sass', 'sass-convert', 'scss']
  spec.files = Dir['rails/init.rb', 'lib/**/*', 'vendor/**/*',
    'bin/*', 'test/**/*', 'extra/**/*', 'Rakefile', 'init.rb',
    '.yardopts'] + readmes
  spec.homepage = 'http://sass-lang.com/'
  spec.has_rdoc = false
  spec.test_files = Dir['test/**/*_test.rb']
end
