#!/usr/bin/env ruby
# git_obj_graph.rb
#
# Generate a Graphviz DOT file (to STDOUT) showing Git objects:
# - commits, trees, blobs (and other objects)
# - ref nodes (HEAD, branches, tags)
# - everything in the repo, but:
#     * objects in the current snapshot of the starting commit are full color
#     * everything else (older/newer history etc.) is faded
#
# Usage:
#   ruby git_obj_graph.rb [REF]
# Examples:
#   ruby git_obj_graph.rb        # uses HEAD
#   ruby git_obj_graph.rb HEAD~2 # uses that commit as the “current state”

require 'open3'
require 'set'

def git(cmd, allow_failure: false)
  out, status = Open3.capture2e("git #{cmd}")
  if !status.success? && !allow_failure
    abort "git #{cmd} failed:\n#{out}"
  end
  out
end

def escape_html(str)
  str.to_s
     .gsub('&', '&amp;')
     .gsub('<', '&lt;')
     .gsub('>', '&gt;')
end

def truncate_lines(str, max_lines: 20, max_width: 80)
  raw_lines = str.lines.map { |l| l.chomp }

  truncated = raw_lines.size > max_lines
  lines = raw_lines.first(max_lines)

  # truncate overly long lines first
  lines.map! do |l|
    l.length > max_width ? l[0, max_width] + '…' : l
  end

  # add explicit truncation marker if we cut off lines
  lines << '… (truncated)' if truncated

  # pad all lines in this node to the same width with spaces
  max_len = lines.map(&:length).max || 0
  lines.map { |l| l.ljust(max_len, ' ') }
end

# lighten hex color by mixing with white
def lighten_color(hex, factor = 0.7)
  h = hex.sub('#', '')
  r = h[0..1].to_i(16)
  g = h[2..3].to_i(16)
  b = h[4..5].to_i(16)
  r = (r + (255 - r) * factor).round
  g = (g + (255 - g) * factor).round
  b = (b + (255 - b) * factor).round
  "#%02x%02x%02x" % [r, g, b]
end

# Parse a single object, fill nodes + edges, and enqueue children
def process_object(oid, nodes, edges, queue)
  type = git("cat-file -t #{oid}").strip
  body = git("cat-file -p #{oid}")
  short = oid[0, 7]

  case type
  when 'commit'
    # Shorten tree/parent hashes, anonymise author, drop committer
    lines = body.lines.map do |l|
      if l.start_with?('tree ')
        parts = l.split
        parts[1] = parts[1][0, 7] if parts[1]
        parts.join(' ') + "\n"
      elsif l.start_with?('parent ')
        parts = l.split
        parts[1] = parts[1][0, 7] if parts[1]
        parts.join(' ') + "\n"
      elsif l.start_with?("author ")
        "author [...]\n"
        # nil
        # if l =~ /^author\s+(.+?)\s+<(.+?)>(.*)$/
        #   rest = $3
        #   "author <anon@example.com> #{rest.strip.split(' ').first}\n"
        # else
        #   "author <anon@example.com>\n"
        # end
      elsif l.start_with?('committer ')
        nil # skip committer line
      else
        l
      end
    end.compact

    content_lines = truncate_lines(lines.join)
    content_html  = content_lines.map { |ln| escape_html(ln) }.join('<BR ALIGN="LEFT"/>')

    label_html  = "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" ALIGN=\"LEFT\">"
    label_html << "<TR><TD ALIGN=\"LEFT\"><B>commit #{escape_html(short)}</B></TD></TR>"
    label_html << "<TR><TD ALIGN=\"LEFT\"><FONT FACE=\"monospace\">#{content_html}</FONT></TD></TR>"
    label_html << "</TABLE>"

    nodes[oid] ||= { type: type, label_html: label_html }

    tree_oid    = nil
    parent_oids = []

    body.each_line do |line|
      if line.start_with?('tree ')
        tree_oid = line.split[1]
      elsif line.start_with?('parent ')
        parent_oids << line.split[1]
      end
    end

    # commit → tree
    if tree_oid
      edges << [oid, tree_oid, 'tree']
      queue << tree_oid
    end

    # commit → parent commit(s)
    parent_oids.each do |p|
      edges << [oid, p, 'parent']
      queue << p
    end

  when 'tree'
    entries = []
    body.each_line do |line|
      if line =~ /^(\d+)\s+(\w+)\s+([0-9a-f]{40})\t(.*)$/
        mode, obj_type, child_oid, name = $1, $2, $3, $4
        short_child = child_oid[0, 7]
        entry = "#{obj_type} #{name} (#{short_child})"
        entries << entry
        edges << [oid, child_oid, name]
        queue << child_oid
      end
    end

    entries_lines = truncate_lines(entries.join("\n"))
    entries_html  = entries_lines.map { |ln| escape_html(ln) }.join('<BR ALIGN="LEFT"/>')

    label_html  = "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" ALIGN=\"LEFT\">"
    label_html << "<TR><TD ALIGN=\"LEFT\"><B>tree #{escape_html(short)}</B></TD></TR>"
    label_html << "<TR><TD ALIGN=\"LEFT\"><FONT FACE=\"monospace\">#{entries_html}</FONT></TD></TR>"
    label_html << "</TABLE>"

    nodes[oid] ||= { type: type, label_html: label_html }

  when 'blob'
    content_lines = truncate_lines(body)
    content_html  = content_lines.map { |ln| escape_html(ln) }.join('<BR ALIGN="LEFT"/>')

    label_html  = "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" ALIGN=\"LEFT\">"
    label_html << "<TR><TD ALIGN=\"LEFT\"><B>blob #{escape_html(short)}</B></TD></TR>"
    label_html << "<TR><TD ALIGN=\"LEFT\"><FONT FACE=\"monospace\">#{content_html}</FONT></TD></TR>"
    label_html << "</TABLE>"

    nodes[oid] ||= { type: type, label_html: label_html }

  else
    content_lines = truncate_lines(body)
    content_html  = content_lines.map { |ln| escape_html(ln) }.join('<BR ALIGN="LEFT"/>')

    label_html  = "<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" ALIGN=\"LEFT\">"
    label_html << "<TR><TD ALIGN=\"LEFT\"><B>#{escape_html(type)} #{escape_html(short)}</B></TD></TR>"
    label_html << "<TR><TD ALIGN=\"LEFT\"><FONT FACE=\"monospace\">#{content_html}</FONT></TD></TR>"
    label_html << "</TABLE>"

    nodes[oid] ||= { type: type, label_html: label_html }
  end
end

# --- starting point (default: HEAD) ---
start_ref    = ARGV[0] || 'HEAD'
start_commit = git("rev-parse #{start_ref}").strip

# --- collect refs (branches, tags, HEAD) ---
ref_nodes        = Set.new          # ref names
ref_commit_edges = Set.new          # [ref_name, oid]
ref_ref_edges    = Set.new          # [from_ref_name, to_ref_name]

# branches + tags
show_ref_output = git("show-ref --heads --tags", allow_failure: true)
show_ref_output.each_line do |line|
  next if line.strip.empty?
  sha, ref = line.strip.split(' ', 2)
  ref_name =
    if ref.start_with?('refs/heads/')
      ref.sub('refs/heads/', '')
    elsif ref.start_with?('refs/tags/')
      "tag: " + ref.sub('refs/tags/', '')
    else
      ref
    end
  ref_nodes << ref_name
  ref_commit_edges << [ref_name, sha]
end

# HEAD
head_commit = start_commit
head_ref = git("symbolic-ref -q HEAD", allow_failure: true).strip
if head_ref.empty?
  # detached HEAD: HEAD → commit
  ref_nodes << 'HEAD'
  ref_commit_edges << ['HEAD', head_commit]
else
  # attached: HEAD → branch, branch → commit (via show-ref)
  branch_name = head_ref.sub('refs/heads/', '')
  ref_nodes << 'HEAD'
  ref_nodes << branch_name unless branch_name.empty?
  ref_ref_edges << ['HEAD', branch_name] unless branch_name.empty?
end

# --- build object graph (nodes + edges) ---
nodes = {}              # oid -> { type:, label_html: }
edges = Set.new         # [from_oid, to_oid, label]
seen  = Set.new

# 1) BFS from starting commit (ensures snapshot objects are present)
queue = [start_commit]
until queue.empty?
  oid = queue.shift
  next if seen.include?(oid)
  seen << oid
  process_object(oid, nodes, edges, queue)
end

# 2) BFS from all ref tips to include the rest of the repo
(ref_commit_edges.map { |_, oid| oid }.uniq).each do |root|
  queue = [root]
  until queue.empty?
    oid = queue.shift
    next if seen.include?(oid)
    seen << oid
    process_object(oid, nodes, edges, queue)
  end
end

# --- compute “snapshot” reachable from start_commit without following parent edges ---
snapshot = Set.new
adj = Hash.new { |h, k| h[k] = [] }

edges.each do |from, to, label|
  adj[from] << [to, label]
end

queue = [start_commit]
until queue.empty?
  oid = queue.shift
  next if snapshot.include?(oid)
  snapshot << oid
  adj[oid].each do |to, label|
    next if label == 'parent'  # don’t walk history backwards
    queue << to
  end
end

# --- compute snapshot edges (reachable via non-parent edges) ---
snapshot_edges = Set.new
queue = [start_commit]
visited = Set.new

until queue.empty?
  oid = queue.shift
  next if visited.include?(oid)
  visited << oid
  edges.each do |from, to, label|
    next unless from == oid
    next if label == 'parent'  # do not follow history
    snapshot_edges << [from, to, label]
    queue << to
  end
end

# --- output DOT to STDOUT ---
f = $stdout

f.puts 'digraph git_objects {'
f.puts '  rankdir=LR;'
f.puts '  node [shape=plain, fontname="monospace"];'

commit_ids = []
tree_ids   = []
blob_ids   = []
other_ids  = []

nodes.each do |oid, data|
  base_color =
    case data[:type]
    when 'commit' then '#5bc0b5'   # commit color
    when 'tree'   then '#d8c59f'   # tree color
    when 'blob'   then '#e7e6e1'   # blob color
    else '#ffffff'
    end

  if snapshot.include?(oid)
    bg_color    = base_color
    border_color = "#000000"
    font_color   = "#000000"
  else
    bg_color    = lighten_color(base_color, 0.7)
    border_color = lighten_color("#000000", 0.7)
    font_color   = lighten_color("#000000", 0.5)
  end

  case data[:type]
  when 'commit' then commit_ids << oid
  when 'tree'   then tree_ids   << oid
  when 'blob'   then blob_ids   << oid
  else               other_ids  << oid
  end

  label_html = data[:label_html]
    .sub('<TABLE ', "<TABLE BGCOLOR=\"#{bg_color}\" COLOR=\"#{border_color}\" ")
    # heading (bold) color
    .gsub('<B>', "<B><FONT COLOR=\"#{font_color}\">")
    .gsub('</B>', '</FONT></B>')
    # body monospace font color
    .gsub('<FONT FACE="monospace">', "<FONT FACE=\"monospace\" COLOR=\"#{font_color}\">")

  f.puts %(  "#{oid}" [label=<#{label_html}>];)
end

# vertical columns per type (since rankdir=LR)
[commit_ids, tree_ids, blob_ids, other_ids].each do |ids|
  next if ids.empty?
  f.puts '  { rank=same; ' + ids.map { |id| %("#{id}") }.join(' ') + ' };'
end

# ref nodes (HEAD, branches, tags)
ref_nodes.each do |ref_name|
  node_id = "ref:#{ref_name}"
  fill_color = 'white'
  fill_color = '#fce94f' if ref_name == 'HEAD'
  f.puts %(  "#{node_id}" [shape=note, style=filled, fillcolor="#{fill_color}", label="#{escape_html(ref_name)}"];)
end

# edges between objects
edges.each do |from, to, label|
  safe_label = escape_html(label.to_s)
  edge_color =
    if snapshot_edges.include?([from, to, label])
      "#000000"
    else
      lighten_color("#000000", 0.7)
    end
  f.puts %(  "#{from}" -> "#{to}" [label="#{safe_label}", color="#{edge_color}"];)
end

# edges from refs to commits
ref_commit_edges.each do |ref_name, oid|
  node_id = "ref:#{ref_name}"
  f.puts %(  "#{node_id}" -> "#{oid}";)
end

# edges from refs to other refs (HEAD → main)
ref_ref_edges.each do |from_ref, to_ref|
  from_id = "ref:#{from_ref}"
  to_id   = "ref:#{to_ref}"
  f.puts %(  "#{from_id}" -> "#{to_id}";)
end

f.puts '}'
