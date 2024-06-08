#!/usr/bin/env ruby

require 'json'
require 'yaml'

data = JSON.parse(`docker network inspect hscode_hscode`)
data[0]['Containers'].each_pair do |k, v|
    name = v['Name']
    tag =  name.split('_').last
    ip = v['IPv4Address'].split('/')[0]
    next if name.index('hscode') == 0
    du = `du -d 0 /user/#{tag}`.split(/\s/).first.to_i
    puts "#{tag} / #{name} => #{ip} (#{du} KiB)"
end
