source "http://rubygems.org"

gemspec

gem 'rake'

# Pin this version since Rubocop occasionally adds new cops in
# incremental releases and we don't want out builds going red because
# of that.
gem 'rubocop', '= 0.14.1' unless RUBY_VERSION =~ /^1.8/

gem 'rubyforge', :group => :development
