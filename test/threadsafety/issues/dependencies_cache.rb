# bundle exec ./multi_threaded_runner.rb -c -s issues/dependencies_cache_setup.rb issues/dependencies_cache.rb
checker = Sass::Plugin::StalenessChecker.new({:load_paths => [$importer]})
10.times do
  (0..100).each do |i|
    deps = checker.send(:dependencies, "foo_#{i}", $importer)
    if rand(100) < 25
      $importer.touch("foo_#{i}")
    end
  end
end
