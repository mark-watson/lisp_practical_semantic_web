require 'pp'
require 'rubygems'
require 'rdfa'
class MyClass
  acts_as_rdfa_parser
end
c = MyClass.new
source = File.open("wikipedia-ex-html", "r").read
results = c.parse(source)
pp results
