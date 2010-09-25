class MockImporter < Sass::Importers::Base
  def initialize(name = "mock")
    @name = name
    @imports = Hash.new({})
  end

  def find_relative(uri, base, options)
    nil
  end

  def find(uri, options)
    contents = @imports[uri][:contents]
    return unless contents
    options[:syntax] = @imports[uri][:syntax]
    options[:filename] = uri
    options[:importer] = self
    @imports[uri][:engine] = Sass::Engine.new(contents, options)
  end

  def mtime(uri, options)
    @imports[uri][:mtime]
  end

  def key(uri, options)
    ["mock", uri]
  end

  def to_s
    @name
  end

  # Methods for testing

  def add_import(uri, contents, syntax = :scss, mtime = Time.now - 10)
    @imports[uri] = {
      :contents => contents,
      :mtime => mtime,
      :syntax => syntax
    }
  end

  def touch(uri)
    @imports[uri][:mtime] = Time.now
  end

  def engine(uri)
    @imports[uri][:engine]
  end
end
