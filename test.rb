require 'rubygems'
require 'dalli'
dc = Dalli::Client.new('localhost:11211')
print dc
#dc.set('abc', 123)
value = dc.get('beuha')
