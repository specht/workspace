#!/usr/bin/env ruby

require 'json'

LIVE_APP_PIN_PATH = '/internal/live_app_pins.json'
LIVE_APP_PIN_MAX_AGE = 60

def active_live_app_fs_tags
    return [] unless File.exist?(LIVE_APP_PIN_PATH)
    return [] if Time.now.to_i - File.mtime(LIVE_APP_PIN_PATH).to_i > LIVE_APP_PIN_MAX_AGE

    data = JSON.parse(File.read(LIVE_APP_PIN_PATH))
    data['fs_tags'] || []
rescue => e
    STDERR.puts "Housekeeping: Could not read live-app pins: #{e.class}: #{e.message}"
    []
end

running_servers = []
inspect = JSON.parse(`docker network inspect workspace_user`)
inspect.first['Containers'].values.each do |container|
    name = container['Name']
    next unless name[0, 8] == 'hs_code_'
    fs_tag = name.sub('hs_code_', '')
    ip = container['IPv4Address'].split('/').first
    running_servers << fs_tag
end

now = Time.now.to_i

live_app_fs_tags = active_live_app_fs_tags
STDERR.puts "Housekeeping: Checking #{running_servers.size} running servers (#{live_app_fs_tags.size} pinned by live apps)..."

running_servers.each do |fs_tag|
    if live_app_fs_tags.include?(fs_tag)
        STDERR.puts "Keeping #{fs_tag} alive because it has an active shared app"
        next
    end

    age = now - Dir["/user/#{fs_tag}/**/*", "/user/#{fs_tag}/**/.*"].reject { |x| File.symlink?(x) }.map { |x| File.mtime(x).to_i }.max
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