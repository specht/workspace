#!/usr/bin/env ruby
# The label printer's station for the Tag der offenen Tür (docs/etiketten.md):
# runs on a laptop at school with the ZPL label printer on its USB port,
# asks the Workspace every two seconds for the next label and writes it to
# the printer. Only Ruby's standard library; the Workspace never connects to
# the laptop, the laptop asks. The command with the server and the token is
# on /etiketten-station, where the administration switches the labels on.
#
#   ruby label-station.rb --server https://workspace.example.org --token TOKEN
#
# The printer: on Linux /dev/usb/lp0 (the default; if writing is refused,
# the user needs the group lp, or start with sudo); a printer of CUPS with
# --lp NAME (sent raw, lp -o raw); --dry-run DIR writes each label into a
# file instead, to try it without a printer. --home replaces the shift of
# the old Hackschule's printer (^LT0^LS-189^LH0,0) if a label sits wrong.
require 'json'
require 'net/http'
require 'open3'
require 'optparse'
require 'uri'

HOME = '^LT0^LS-189^LH0,0'

options = { :device => '/dev/usb/lp0', :interval => 2.0 }
OptionParser.new do |o|
    o.banner = 'Usage: ruby label-station.rb --server URL --token TOKEN [--device PATH | --lp NAME | --dry-run DIR]'
    o.on('--server URL', 'the Workspace, as on /etiketten-station') { |v| options[:server] = v.chomp('/') }
    o.on('--token TOKEN', 'the station token from /etiketten-station') { |v| options[:token] = v }
    o.on('--device PATH', 'the printer device (default /dev/usb/lp0)') { |v| options[:device] = v }
    o.on('--lp NAME', 'a CUPS printer, sent raw') { |v| options[:lp] = v }
    o.on('--dry-run DIR', 'write the labels into files in DIR') { |v| options[:dry_run] = v }
    o.on('--home ZPL', "replaces #{HOME} in each label") { |v| options[:home] = v }
end.parse!
abort 'Missing --server or --token (both are on /etiketten-station).' unless options[:server] && options[:token]

def call(options, path, data)
    uri = URI("#{options[:server]}/api/labels/#{path}")
    response = Net::HTTP.start(uri.host, uri.port, :use_ssl => uri.scheme == 'https', :open_timeout => 10, :read_timeout => 20) do |http|
        http.post(uri.path, data.merge(:token => options[:token]).to_json, 'Content-Type' => 'application/json')
    end
    abort 'The Workspace refuses the token: are the labels still switched on? Take the new command from /etiketten-station.' if response.code == '403'
    raise "#{path}: HTTP #{response.code}" unless response.code == '200'

    JSON.parse(response.body)
end

def print_label(options, id, zpl)
    zpl = zpl.sub(HOME, options[:home]) if options[:home]
    if options[:dry_run]
        Dir.mkdir(options[:dry_run]) unless Dir.exist?(options[:dry_run])
        File.write(File.join(options[:dry_run], "#{Time.now.strftime('%H%M%S')}-#{id}.zpl"), zpl)
    elsif options[:lp]
        _out, error, status = Open3.capture3('lp', '-d', options[:lp], '-o', 'raw', :stdin_data => zpl)
        raise "lp: #{error.strip}" unless status.success?
    else
        File.open(options[:device], 'wb') { |f| f.write(zpl) }
    end
end

target = options[:dry_run] ? "files in #{options[:dry_run]}" : (options[:lp] ? "CUPS printer #{options[:lp]}" : options[:device])
puts "Asking #{options[:server]} for labels, printing to #{target}. Stop with Ctrl+C."
printed = 0
trap('INT') { puts "\n#{printed} labels printed."; exit }
loop do
    begin
        job = call(options, 'next', {})
        if job['id']
            print_label(options, job['id'], job['zpl'])
            call(options, 'done', { :id => job['id'] })
            printed += 1
            puts "#{Time.now.strftime('%H:%M:%S')}  label #{printed} printed"
            next
        end
    rescue Errno::EACCES
        abort "Writing to #{options[:device]} is refused: add your user to the group lp (sudo usermod -aG lp $USER, then log in again) or start with sudo."
    rescue Errno::ENOENT
        abort "#{options[:device]} does not exist: is the printer switched on and plugged in? (ls /dev/usb)"
    rescue StandardError => e
        warn "#{Time.now.strftime('%H:%M:%S')}  #{e.message}; trying again"
        sleep 5
    end
    sleep options[:interval]
end
