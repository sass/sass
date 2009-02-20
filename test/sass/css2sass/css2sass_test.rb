require 'test/unit'
require File.dirname(__FILE__) + '/../../../lib/sass/css'

class CSS2SassTest < Test::Unit::TestCase
  def test_basic
    css = <<-CSS
h1 {
  color: red;
}
    CSS
    sass = <<-SASS
h1
  :color red
    SASS
    sass_alt = <<-SASS_ALT
h1
  color: red
SASS_ALT
    assert_equal(sass, css2sass(css))
    assert_equal(sass_alt, css2sass(css, :alternate => true))
  end

  def test_nesting_1
    css = <<-CSS
li {
  display: none;
}

li a {
  text-decoration: none;
}

li a span {
  color: yellow;
}

li a:hover {
  text-decoration: underline;
}
CSS

    sass = <<-SASS
li
  :display none

  a
    :text-decoration none

    span
      :color yellow

    &:hover
      :text-decoration underline
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_nesting_2
    css = <<-CSS
li.menu {
  display: none;
}

li.menu a {
  text-decoration: none;
}

li.menu a:hover {
  text-decoration: underline;
}

li a:hover {
  color: red;
}
CSS

    sass = <<-SASS
li.menu
  :display none

  a
    :text-decoration none

    &:hover
        :text-decoration underline

  a:hover
    :color red
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_comments_oneline
    css = <<-CSS
// This is a comment!
foo {
  bar: baz;
}

monkey { // This is another comment!
  tree: true;
}

div.banana {
  // Who doesn't like bananas
  potassium: high;
}

#pomegranate {
  omnom: nom; // fruity goodness
}

.arewethereyet {
  almost: there;
} // hum...
CSS
    sass = <<-SASS
foo
  :bar baz


monkey
  :tree true


div.banana
  :potassium high


.arewethereyet
  :almost there
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_comments_multiline
    css = <<-CSS
/* comment */
elephant.rawr {
  rampages: excessively;
}

/* actual multiline
  comment */
span.turkey {
  isdinner: true;
}

.turducken {
  /* Sounds funny
     doesn't it */
  chimera: not_really;
}

#overhere {
  bored: sorta; /* it's for a good
  cause */
  better_than: thread_pools;
}

#one_more {
  finally: srsly;
} /* just a line here */
CSS
    sass = <<-SASS
elephant.rawr
  :rampages excessively


span.turkey
  :isdinner true


.turducken
  :chimera not_really


#overhere
  :bored sorta
  :better_than thread_pools


#one_more
  :finally srsly
SASS
    assert_equal(css2sass(css), sass)
  end

  def test_fold_commas_1
    css = <<-CSS
li .one {
  color: red;
}
li .two {
  color: red;
}
CSS
    sass = <<-SASS
li
  .one, .two
    :color red
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_fold_commas_2
    css = <<-CSS
li .one {
  color: red;
}
li .two {
  color: red;
}
li .three {
  color: red;
  mu: fasa;
}
CSS
    sass = <<-SASS
li
  .one, .two, .three
    :color red
  .three
    :mu fasa
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_fold_commas_3
    css = <<-CSS
.one, .two {
  color: green;
}

.two, .three {
  color: red;
}
CSS
    sass = <<-SASS
.one
  :color green


.two
  :color green
  :color red


.three
  :color red
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_bad_formatting
    css = <<-CSS
hello {
  parent: true;
}

hello  there {
  parent: false;
}
hello who  {
  hoo: false;
}
hello why {
   y: true;
}
hello when {
  wen:  nao;
}


down_here { yeah: true; }
CSS
    sass = <<-SASS
hello
  :parent true

  there
    :parent false

  who
    :hoo false

  why
    :y true

  when
    :wen nao


down_here
  :yeah true
SASS
    assert_equal(sass, css2sass(css))
  end

  private
  def css2sass(string, opts={})
    Sass::CSS.new(string, opts).render
  end
end