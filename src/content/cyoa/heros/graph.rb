#!/usr/bin/env ruby

# Configuration
MARKDOWN_DIR = 'pages/'  # Directory containing your .md files
OUTPUT_DOT = 'adventure.dot'

def hue_to_rgb(p, q, t)
  t += 1 if t < 0
  t -= 1 if t > 1
  
  if t < 1/6.0
    p + (q - p) * 6 * t
  elsif t < 1/2.0
    q
  elsif t < 2/3.0
    p + (q - p) * (2/3.0 - t) * 6
  else
    p
  end
end

def group_color(group)
  # Convert HSL to RGB (simplified conversion)
  hash = group.each_byte.reduce(0) { |a, b| (a << 5) - a + b }
  hue = hash % 360
  h = hue / 360.0
  s = 0.7
  l = 0.5
  
  # HSL to RGB conversion
  q = l < 0.5 ? l * (1 + s) : l + s - l * s
  p = 2 * l - q
  
  r = hue_to_rgb(p, q, h + 1/3.0)
  g = hue_to_rgb(p, q, h)
  b = hue_to_rgb(p, q, h - 1/3.0)
  
  "#%02x%02x%02x" % [(r * 255).round, (g * 255).round, (b * 255).round]
end

def group_fillcolor(group)
  # Lighter version (same hue, less saturation)
  hash = group.each_byte.reduce(0) { |a, b| (a << 5) - a + b }
  hue = hash % 360
  h = hue / 360.0
  s = 0.5
  l = 0.9
  
  q = l < 0.5 ? l * (1 + s) : l + s - l * s
  p = 2 * l - q
  
  r = hue_to_rgb(p, q, h + 1/3.0)
  g = hue_to_rgb(p, q, h)
  b = hue_to_rgb(p, q, h - 1/3.0)
  
  "#%02x%02x%02x" % [(r * 255).round, (g * 255).round, (b * 255).round]
end

def node_color(group)
  # Medium version (same hue, medium lightness)
  hash = group.each_byte.reduce(0) { |a, b| (a << 5) - a + b }
  hue = hash % 360
  h = hue / 360.0
  s = 0.7
  l = 0.8
  
  q = l < 0.5 ? l * (1 + s) : l + s - l * s
  p = 2 * l - q
  
  r = hue_to_rgb(p, q, h + 1/3.0)
  g = hue_to_rgb(p, q, h)
  b = hue_to_rgb(p, q, h - 1/3.0)
  
  "#%02x%02x%02x" % [(r * 255).round, (g * 255).round, (b * 255).round]
end

def extract_metadata(content)
  if match = content.match(/<!--\s*(.*?)\s*--\s*(.*?)\s*-->/)
    {
      group: match[1].strip,
      summary: match[2].strip
    }
  elsif match = content.match(/<!--\s*(.*?)\s*-->/)
    {
      group: match[1].strip,
    }
  else
    { group: '' }
  end
end

def extract_links(content)
  content.scan(/\[.*?\]\(#(.+)\)/).flatten.uniq
end

# Collect all page data
pages = {}
Dir.glob(File.join(MARKDOWN_DIR, '*.md')).each do |file|
  # next unless file =~ /\d+\.md$/  # Only process files like 1.md, 2.md etc.
  
  page_num = File.basename(file, '.md')
  content = File.read(file)
  
  metadata = extract_metadata(content)
  pages[page_num] = {
    group: metadata[:group],
    summary: metadata[:summary],
    links: extract_links(content)
  }
end

# Generate DOT file
File.open(OUTPUT_DOT, 'w') do |f|
  f.puts <<~DOT
  digraph Adventure {
    rankdir="TB"
    graph [fontname="Arial"]
    node [shape=box, style=filled, fontname="Arial"]
    edge [fontname="Arial"]
    
    # Define group colors
  DOT

  # Create subgraphs for each group
  groups = pages.values.map { |p| p[:group] }.uniq
  groups.each do |group|
    next if group.empty?  # Skip empty groups
    f.puts <<~DOT
      subgraph cluster_#{group.downcase.gsub(/\s+/, '_')} {
        label = "#{group}"
        color = "#{group_color(group)}"
        style = "filled"
        fillcolor = "#{group_fillcolor(group)}"
    DOT

    # Add nodes in this group
    pages.each do |page_num, data|
      if data[:group] == group
        label = if data[:summary]
                  "#{page_num}\n#{data[:summary]}"
                else
                  "#{page_num}"
                end
        f.puts %(      "#{page_num}" [label="#{label.gsub('"', '\"')}", fillcolor="#{node_color(group)}"])
      end
    end

    f.puts "    }"
  end

  # Add all edges
  f.puts "\n  # Links between pages"
  pages.each do |source, data|
    data[:links].each do |target|
      if pages.key?(target)
        f.puts %(  "#{source}" -> "#{target}";)
      else
        puts "Warning: Broken link from #{source} to #{target}"
      end
    end
  end

  f.puts "}"
end

puts "DOT file generated: #{OUTPUT_DOT}"
puts "To generate PNG: dot -Tpng #{OUTPUT_DOT} -o adventure.png"
