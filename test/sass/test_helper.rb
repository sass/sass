test_dir = File.dirname(__FILE__)
$:.unshift test_dir unless $:.include?(test_dir)

class Test::Unit::TestCase
  def absolutize(file)
    "#{File.dirname(__FILE__)}/#{file}"
  end
end
