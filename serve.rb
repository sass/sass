require 'rubygems'
require 'sinatra'
require 'sinatra/dynamicmatic'
require 'timeout'
require 'date'

get('/try.html') {
  redirect "http://trysass.com"
}