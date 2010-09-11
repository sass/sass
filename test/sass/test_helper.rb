class Test::Unit::TestCase
  def absolutize(file)
    "#{File.dirname(__FILE__)}/#{file}"
  end
end