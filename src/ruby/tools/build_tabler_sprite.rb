#!/usr/bin/env ruby
# Builds src/static/include/tabler/tabler.svg, a sprite of the Tabler icons
# (https://tabler.io/icons, MIT) listed in src/static/include/tabler/icons.txt.
#
#   git clone --depth 1 https://github.com/tabler/tabler-icons.git /tmp/tabler-icons
#   ruby src/ruby/tools/build_tabler_sprite.rb /tmp/tabler-icons
#
# icons.txt names one icon per line as <style>/<name>, e.g. outline/check.
# In HTML: <svg class='ti'><use href='/include/tabler/tabler.svg#check'/></svg>
# (filled icons get the id <name>-filled).
require 'fileutils'

checkout = ARGV[0] or abort "usage: #{$0} <path to tabler-icons checkout>"
dir = File.expand_path('../../static/include/tabler', __dir__)
names = File.readlines(File.join(dir, 'icons.txt'), chomp: true).map(&:strip).reject { |l| l.empty? || l.start_with?('#') }
version = File.read(File.join(checkout, 'package.json'))[/"version":\s*"([^"]+)"/, 1]
commit = `git -C #{checkout} rev-parse HEAD 2>/dev/null`.strip

symbols = names.map do |entry|
    style, name = entry.split('/', 2)
    svg = File.read(File.join(checkout, 'icons', style, "#{name}.svg"))
    svg = svg.sub(/<!--.*?-->\s*/m, '')
    attrs = svg[/<svg(.*?)>/m, 1]
    body = svg[/<svg.*?>(.*)<\/svg>/m, 1].strip.gsub(/\s*\n\s*/, '')
    keep = attrs.scan(/(viewBox|fill|stroke|stroke-width|stroke-linecap|stroke-linejoin)="([^"]*)"/)
    id = style == 'filled' ? "#{name}-filled" : name
    "<symbol id=\"#{id}\" #{keep.map { |k, v| "#{k}=\"#{v}\"" }.join(' ')}>#{body}</symbol>"
end
out = +"<svg xmlns=\"http://www.w3.org/2000/svg\">\n"
out << "<!-- Tabler Icons #{version} (#{commit[0, 12]}), MIT License, see LICENSE. Built by src/ruby/tools/build_tabler_sprite.rb -->\n"
symbols.each { |s| out << s << "\n" }
out << "</svg>\n"
File.write(File.join(dir, 'tabler.svg'), out)
FileUtils.cp(File.join(checkout, 'LICENSE'), File.join(dir, 'LICENSE'))
puts "#{symbols.size} icons written to #{File.join(dir, 'tabler.svg')}"
