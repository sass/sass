require '.haml/lib/haml'
require 'rubygems'
gem 'sinatra-sinatra'
require 'sinatra'
require 'sinatra/dynamicmatic'

get('/try.html') {haml :try}

post('/try.html') do
  begin
    @result = Haml::Engine.new(params[:input], :suppress_eval => true).render
  rescue Haml::SyntaxError => e
    @result = "Haml Error: " + e
  end

  haml :try
end
