test_dir = File.dirname(__FILE__)
$:.unshift test_dir unless $:.include?(test_dir)

class MiniTest::Test
  def absolutize(file)
    File.expand_path("#{File.dirname(__FILE__)}/#{file}")
  end
end
