#!/usr/bin/env ruby

require 'json'
require 'yaml'

data = JSON.parse(`docker network inspect workspace`)
data[0]['Containers'].each_pair do |k, v|
    name = v['Name']
    tag =  name.split('_').last
    ip = v['IPv4Address'].split('/')[0]
    next if name.index('workspace') == 0
    du = `du -d 0 /user/#{tag}`.split(/\s/).first.to_i
    puts "#{tag} / #{name} => #{ip} (#{du} KiB)"
end
