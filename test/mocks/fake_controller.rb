require File.dirname(__FILE__) + '/../../lib/sass/engine'
require File.dirname(__FILE__) + '/../../lib/sass/plugin'

class FakeController
  include Sass::Plugin
end
