require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/engine'
$:.unshift File.join(File.dirname(__FILE__), "..", "lib")
require 'rubygems'
require 'action_view'

class HamlTest < Test::Unit::TestCase
  include HAMLHelpers

  def test_tupleize
    assert_equal(tupleize([1,2,3], [4,5,6]), [[1,4],[2,5],[3,6]])
  end

  def test_find_and_flatten
    assert_equal(find_and_flatten("<br/><textarea></textarea><br/>"),
                                  "<br/><textarea></textarea><br/>")
    assert_equal(find_and_flatten("<code lang='ruby'>TEST!</code>\t\t<p></p>"),
                                  "<code lang='ruby'>TEST!</code>\t\t<p></p>")
    assert_equal(find_and_flatten("<pre>Hello\nWorld!\nYOU ARE \rFLAT?\n\rOMGZ!</pre></br>"),
                                  "<pre>Hello&#x000A;World!&#x000A;YOU ARE FLAT?&#x000A;OMGZ!</pre></br>")
    assert_equal(                 "<div class='text_area_test_area'>\n  <textarea>Two&#x000A;  lines</textarea>\n</div>\n",
                 find_and_flatten("<div class='text_area_test_area'>\n  <textarea>Two\n  lines</textarea>\n</div>\n"))
    assert_equal(                 "<code>Two&#x000A;lines</code><pre>a&#x000A;b&#x000A;c</pre>",
                 find_and_flatten("<code>Two\nlines</code><pre>a\nb\nc</pre>"))
  end
end
