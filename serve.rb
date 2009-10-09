require '.haml/lib/haml'
require 'rubygems'
gem 'sinatra-sinatra'
require 'sinatra'
require 'sinatra/dynamicmatic'
require 'timeout'

get('/try.html') {haml :try}

post('/try.html') do
  begin
    Timeout.timeout(5) do
      @result = Haml::Engine.new(params[:input], :suppress_eval => true).render
    end
  rescue Haml::SyntaxError => e
    @result = "Haml Error: " + e
  rescue Timeout::Error
    @result = "Timed out!"
  end

  haml :try
end
