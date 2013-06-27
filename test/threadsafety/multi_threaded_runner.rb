#!/usr/bin/env ruby

class MultiThreadedRunner
  DEFAULT_THREAD_COUNT = 4
  DEFAULT_WAIT_TIME = 10
  def initialize(thread_count = DEFAULT_THREAD_COUNT, &work_to_do)
    @thread_count = thread_count
    @work_to_do = work_to_do
  end

  def run(wait_time = DEFAULT_WAIT_TIME)
    start_time = Time.now
    threads = (0...@thread_count).map{|threadnumber| Thread.new(&@work_to_do) }
    threads.reject! do |t|
      wait = [0.01, wait_time - (Time.now - start_time)].max
      t.join(wait)
    end
    if threads.any?
      raise RuntimeError, "#{threads.size} threads failed to complete in #{@wait_time} seconds"
    end
  end
end

class MultiThreadedScriptRunner < MultiThreadedRunner
  def initialize(script_path, thread_count = DEFAULT_THREAD_COUNT)
    script_contents = File.read(script_path)
    super(thread_count) do
      instance_eval script_contents, script_path
    end
  end
end

if File.expand_path($0) == File.expand_path(__FILE__)
  require 'optparse'
  options = {
    :threads => MultiThreadedRunner::DEFAULT_THREAD_COUNT,
    :script => nil,
    :wait_time => MultiThreadedRunner::DEFAULT_WAIT_TIME,
    :clear_cache => true
  }

  parser = OptionParser.new do |opts|
    opts.banner = "Usage: test_threadsafety.rb [options] path/to/ruby/script.rb"
    opts.on("-t", "--threads NUMBER_OF_THREADS", Integer, "How many threads to run concurrently.") do |threads|
      options[:threads] = threads
    end
    opts.on("-w", "--wait NUMBER_OF_SECONDS", Float, "How long to wait for each script to complete (in seconds).") do |seconds|
      options[:wait_time] = seconds
    end
    opts.on("-s", "--setup START_UP_SCRIPT", String, "A ruby script to load before starting the threads.") do |path|
      load path
    end
    opts.on("-c", "--[no-]clear-cache", "Clear the cache before running (default true).") do |clear|
      options[:clear_cache] = clear
    end
    opts.on_tail("-h", "--help", "Print this message") do
      puts opts
    end
  end
  parser.parse!

  options[:script] ||= ARGV.first

  if options[:clear_cache]
    require 'fileutils'
    FileUtils.rm_rf(".sass-cache")
  end

  runner = MultiThreadedScriptRunner.new(options[:script], options[:threads])
  begin
    runner.run(options[:wait_time])
  rescue RuntimeError => e
    $stderr.puts e.message
    exit 1
  end
end
