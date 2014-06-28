source "http://rubygems.org"

gemspec

gem 'rake'

# Pin this version since Rubocop occasionally adds new cops in
# incremental releases and we don't want out builds going red because
# of that.
gem 'rubocop', '= 0.18.0' unless RUBY_VERSION =~ /^1\.8/

if RUBY_VERSION =~ /^1\.8/
  gem 'listen', '~> 1.1.0'
elsif RUBY_VERSION =~ /^1\.9\.[012]$/
  gem 'listen', '>= 1.1.0', '< 2.0.0'
else
  gem 'listen', '>= 1.1.0', '< 3.0.0'
end

gem 'rubyforge', :group => :development
gem 'minitest', :group => :test
