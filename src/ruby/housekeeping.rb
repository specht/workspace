#!/usr/bin/env ruby

require 'json'

running_servers = []
inspect = JSON.parse(`docker network inspect bridge`)
inspect.first['Containers'].values.each do |container|
    name = container['Name']
    next unless name[0, 8] == 'hs_code_'
    fs_tag = name.sub('hs_code_', '')
    ip = container['IPv4Address'].split('/').first
    running_servers << fs_tag
end

now = Time.now.to_i

STDERR.puts "Housekeeping: Checking #{running_servers.size} running servers..."

running_servers.each do |fs_tag|
    age = now - Dir["/user/#{fs_tag}/**/*", "/user/#{fs_tag}/**/.*"].map { |x| File.mtime(x).to_i }.max
    if age > 60 * 180
        STDERR.puts "Killing #{fs_tag} => #{age} seconds old"
        system("docker kill hs_code_#{fs_tag}")
    end
end

du_for_fs_tag = {}

Dir["/user/*"].each do |path|
    fs_tag = File.basename(path)
    du = `du -d 0 /user/#{fs_tag}`.split(/\s/).first.to_i
    du_for_fs_tag[fs_tag] = du
end

File.open('/internal/du_for_fs_tag.json', 'w') do |f|
    f.write(du_for_fs_tag.to_json)
end
