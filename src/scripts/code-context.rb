#!/usr/bin/env ruby
# Proposes context lines (»~ «, docs/aktivitaet-schreiben.md) for the code
# blocks of an activity page: each block is looked up in the files a
# screenshot recipe writes (write-file: … <- file:…), and up to three
# non-blank lines before and after it become context, with the nesting of
# the file. Code that later blocks of the same step add is left out, and so
# is a comment right above it: the context shows the file as the student has
# it at this point of the page.
#
#   src/scripts/code-context.rb PAGE.md [options]
#
#   --apply        write the proposals into the page (without: only print them)
#   --lines N      lines of context before and after (3)
#   --only L,L     only the blocks whose opening ``` is on these lines
#   --previous     the stage of the recipe right before the block comes
#                  first (pages that show the result before the code)
#   --existing     lines the student has already (from an earlier block of
#                  the step), found once in the file, stay and become context
#                  in the block (headings of an outline filled in later)
#
# Read every proposal: a block that shows a change rather than an insertion,
# or a file that the recipe writes in a later order than the page builds it,
# needs a decision by hand. Blocks with context, elisions (…) or that are a
# whole file are left as they are.

def option(name)
    i = ARGV.index(name)
    i ? ARGV[i + 1] : nil
end

page = ARGV.first
unless page && File.exist?(page)
    STDERR.puts "Usage: #{$0} PAGE.md [--apply] [--lines N] [--only L,L] [--previous] [--existing]"
    exit 1
end
APPLY = ARGV.include?('--apply')
PREVIOUS = ARGV.include?('--previous')
EXISTING = ARGV.include?('--existing')
N = (option('--lines') || 3).to_i
ONLY = option('--only')&.split(',')&.map(&:to_i)
BASE = File.dirname(page)
LINES = File.read(page).split("\n", -1)

CONTEXT = /\A~( |\z)/
ELISION = /\A\s*(\/\/|#|%|<!--|\/\*)?\s*(…|\.\.\.)\s*(-->|\*\/)?\s*\z/
SKIP = %w(bash sh console text shell plaintext)
WRITE = /\Awrite-file:\s*(\S+)\s*<-\s*file:(\S+)/
RECIPE = '<!-- tutorial-'
# a comment line, by the extension of the file
COMMENT = {
    '.c' => /\A\s*\/\//, '.h' => /\A\s*\/\//, '.js' => /\A\s*\/\//, '.ts' => /\A\s*\/\//,
    '.css' => /\A\s*\/\*.*\*\/\s*\z/, '.html' => /\A\s*<!--.*-->\s*\z/,
    '.svelte' => /\A\s*(\/\/|<!--.*-->\s*\z|\/\*.*\*\/\s*\z)/,
    '.tex' => /\A\s*%/, '.rb' => /\A\s*#(?!!)/, '.py' => /\A\s*#(?!!)/, '.lua' => /\A\s*--/,
}

def read(path)
    File.read(path, encoding: 'utf-8').scrub.split("\n", -1)
end

# The files the first recipe after line start writes: [[target, source]]
def recipe_files(start)
    i = start
    i += 1 while i < LINES.size && !LINES[i].start_with?(RECIPE)
    files = []
    while i < LINES.size
        if (m = WRITE.match(LINES[i]))
            files << [m[1], File.join(BASE, m[2])]
        end
        if LINES[i].include?('-->') && i > start
            return files unless files.empty?
            # a recipe without files: look further
            i += 1
            i += 1 while i < LINES.size && !LINES[i].start_with?(RECIPE)
            next
        end
        i += 1
    end
    files
end

# The files the last recipe before line start writes
def previous_files(start)
    k = start
    k -= 1 while k >= 0 && !LINES[k].start_with?(RECIPE)
    files = []
    while k >= 0 && k < start
        if (m = WRITE.match(LINES[k]))
            files << [m[1], File.join(BASE, m[2])]
        end
        break if LINES[k].include?('-->')
        k += 1
    end
    files
end

# What the last recipe before line start wrote to target
def written_before(start, target)
    found = nil
    (0...start).each do |k|
        m = WRITE.match(LINES[k])
        found = File.join(BASE, m[2]) if m && m[1] == target
    end
    found ? read(found) : []
end

# The blocks before the next recipe form a step
def step_of(start)
    k = start
    k += 1 while k < LINES.size && !LINES[k].start_with?(RECIPE)
    k
end

def indent(s)
    s.size - s.sub(/\A[ \t]+/, '').size
end

def find(block, file)
    want = block.map(&:strip)
    have = file.map(&:strip)
    (0..have.size - want.size).select { |p| have[p, want.size] == want }
end

def without_trailing_blanks(lines)
    lines = lines.dup
    lines.pop while lines.any? && lines.last.strip.empty?
    lines
end

# All code blocks: [start line, end line, body]
blocks = []
k = 0
while k < LINES.size
    if (m = /\A```(\w+?)((?:_\w+)*)\s*\z/.match(LINES[k]))
        e = k + 1
        e += 1 until LINES[e].start_with?('```')
        blocks << [k, e, LINES[k + 1...e]] unless SKIP.include?(m[1])
        k = e
    end
    k += 1
end

proposals = {}
blocks.each_with_index do |(start, stop, body), n|
    next if ONLY && !ONLY.include?(start + 1)
    next if body.empty? || body.any? { |l| l =~ CONTEXT || l =~ ELISION }
    body = without_trailing_blanks(body)
    later = blocks[n + 1..].select { |b| step_of(b[0]) == step_of(start) }
    candidates = (PREVIOUS ? previous_files(start) : []) + recipe_files(stop)
    candidates.each do |target, source|
        f = read(source)
        hits = find(body, f)
        next unless hits.size == 1
        p = hits.first
        q = p + body.size
        last = f.rindex { |l| !l.strip.empty? }
        break if p == 0 && q > last # the whole file
        comment = COMMENT[File.extname(target)] || /\A\s*\/\//

        had = written_before(start, target).map(&:strip)
        blocks[0...n].each do |b|
            had += b[2].reject { |l| l =~ CONTEXT }.map(&:strip) if step_of(b[0]) == step_of(start)
        end
        once = ->(t) { !t.empty? && f.count { |l| l.strip == t } == 1 }
        keep = ->(i) { EXISTING && had.include?(f[i].strip) && once.(f[i].strip) }

        # what the student does not have yet: the code of later blocks
        removed = {}
        later.each do |_, _, lb|
            lb = without_trailing_blanks(lb.reject { |l| l =~ CONTEXT })
            next if lb.empty?
            lh = find(lb, f)
            next unless lh.size == 1 && !(lh.first <= p && p < lh.first + lb.size)
            (lh.first...lh.first + lb.size).each { |i| removed[i] = true unless keep.(i) }
            # a comment right above the later code belongs to it
            c = lh.first - 1
            while c >= 0 && f[c] =~ comment
                removed[c] = true
                c -= 1
            end
        end
        # a comment right above the new code that the student does not have yet
        c = p - 1
        while c >= 0 && f[c] =~ comment && !had.include?(f[c].strip)
            removed[c] = true
            c -= 1
        end

        take = lambda do |range|
            got = []
            nonblank = 0
            range.each do |i|
                break if nonblank == N
                next if removed[i]
                next if f[i].strip.empty? && got.any? && got.last.strip.empty?
                got << f[i]
                nonblank += 1 unless f[i].strip.empty?
            end
            got
        end
        before = take.((p - 1).downto(0)).reverse
        after = take.(q...f.size)
        before.shift while before.any? && before.first.strip.empty?
        after.pop while after.any? && after.last.strip.empty?
        break if before.empty? && after.empty?

        # as in the file, less the indentation all lines share
        common = (before + f[p...q] + after).reject { |l| l.strip.empty? }.map { |l| indent(l) }.min
        cut = ->(l) { l.strip.empty? ? '' : l[common..] }
        ctx = ->(ls) { ls.map { |l| l.strip.empty? ? '~' : "~ #{cut.(l)}" } }
        middle = (p...q).map { |i| keep.(i) ? "~ #{cut.(f[i])}" : cut.(f[i]) }
        proposals[start] = [target, ctx.(before) + middle + ctx.(after)]
        break
    end
end

out = []
i = 0
while i < LINES.size
    if proposals[i]
        e = i + 1
        e += 1 until LINES[e].start_with?('```')
        target, body = proposals[i]
        out << LINES[i]
        out.concat(body)
        out << LINES[e]
        puts "--- line #{i + 1} (#{target})"
        puts body
        i = e + 1
        next
    end
    out << LINES[i]
    i += 1
end

STDERR.puts "#{proposals.size} blocks"
File.write(page, out.join("\n")) if APPLY
