require 'rubygems'
require 'sinatra'
require 'json'
set :port, 3123
set :environment, :production
Dir.chdir(File.dirname(__FILE__) + "/..")

post "/" do
  payload = JSON.parse(params["payload"])

  if payload["ref"] == "refs/heads/master"
    system("rake release_edge &> edge-gem-output.log")
  elsif payload["ref"] =~ %r{^refs/heads/(haml|sass)-pages$}
    system("rake pages PROJ=#{$1}")
  end
end
