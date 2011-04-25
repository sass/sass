require '.haml/lib/haml'
require '.sass/lib/sass'
require 'rubygems'
require 'sinatra'
require 'sinatra/dynamicmatic'
require 'timeout'
require 'date'

get('/try.html') {haml :try}

post('/try.html') do
  begin
    Timeout.timeout(5) do
      @result = Sass::Engine.new(params[:input]).render
    end
  rescue Sass::SyntaxError => e
    @result = "Sass Error: " + e
  rescue Timeout::Error
    @result = "Timed out!"
  end

  haml :try
end
