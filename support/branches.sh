#!/bin/bash
TMPFILE="/tmp/build.starnet.fi-branches"

echo "this is for build.starnet.fi:"
wget https://build.qvantel.net/api/json/ --no-check-certificate --user $USER --ask-password --quiet -O $TMPFILE

echo "give password for Luna:"
read -s password

ruby <<EOF
require 'rubygems'
require 'json'
require 'xmlrpc/client'

real_branches=\`hg branches | cut -f 1 -d ' '\`.split("\n")

begin
  test_branches=JSON.parse(File.read('/tmp/build.starnet.fi-branches'))['jobs'].select { |job| job['name'] =~ /^yoigo/ }.map{ |b| b['name'].gsub(/^yoigo-/,'') }

  if (test_branches-real_branches).length>0
    puts "== These branches exist in Hudson but do not exist in HG:"
    puts test_branches-real_branches
  end
  if (real_branches-test_branches).length>0
    puts "== These branches exist in HG but do not exist in Hudson:"
    puts real_branches-test_branches
  end
rescue Exception => e
  puts "hudson branch checking failed: "+e.message
end

server = XMLRPC::Client.new2("https://luna.qvantel.net/rpc/xmlrpc")

token = server.call("confluence1.login", "$USER","$password")
page = server.call("confluence1.getPage", token, "2861367")
content = page['content']
rows = content.gsub("\r",'').split("\n")
rows = rows.select { |row| row =~ /\A\|/ }
branches_in_rm = []
rows.each do |row|
  elements = row.split('|')
  first = elements[1].gsub('\\\','').strip
  if first.length > 0
    branches_in_rm << first
  end
end

if (branches_in_rm-real_branches).length>0
  puts "== These branches exist in Release management page in Luna but do not exist in HG:"
  puts branches_in_rm-real_branches
end
if (real_branches-branches_in_rm).length>0
  puts "== These branches exist in HG but do not exist in Release management page:"
  puts real_branches-branches_in_rm
end

EOF

rm $TMPFILE
