#!/usr/bin/env ruby

require 'json'
require 'pathname'

repo_root = File.expand_path('../..', __dir__)
content_root = File.join(repo_root, 'src', 'content')
USAGE = <<~TEXT
    Generated tutorial screenshots and videos: render them, or see their state.

    Usage:
      src/scripts/recreate-screenshots.rb TUTORIAL           render every generated image and video of one tutorial
      src/scripts/recreate-screenshots.rb --all              the same for every tutorial with recipes
      src/scripts/recreate-screenshots.rb --stale [TUTORIAL] render only what is out of date (all tutorials, or one)
      src/scripts/recreate-screenshots.rb --status [TUTORIAL] render nothing; show the state of each generated file
                                                             and how many images of each page are generated or manual
      src/scripts/recreate-screenshots.rb --help             this text

    TUTORIAL is a folder below src/content (bif, lehrmaterial/lehrmaterial-2d)
    or a Markdown path. Drafts in src/content/_drafts are skipped. Every run ends
    with the state of the tutorials it rendered:

      ✓ current   ✗ stale (rendered from an older recipe or setup)   ✗ missing

    The development services must be running. Rendering on page views may be
    paused (status bar of a tutorial page); this script renders either way.
TEXT

mode = :force
args = ARGV.dup
case args.first
when '--help', '-h'
    puts USAGE
    exit 0
when '--all'
    mode = :all
    args.shift
when '--stale'
    mode = :stale
    args.shift
when '--status'
    mode = :status
    args.shift
end
abort USAGE if args.size > 1 || (mode == :force && args.empty?) || (mode == :all && !args.empty?)
abort USAGE if args.first.to_s.start_with?('-')

content_path = Pathname.new(content_root).realpath
RECIPE = '<!-- tutorial-screenshot'

relative_of = lambda do |path|
    Pathname.new(path).realpath.relative_path_from(content_path).to_s
end

resolve = lambda do |argument|
    candidates = if argument.end_with?('.md')
        [
            File.expand_path(argument, repo_root),
            File.expand_path(argument, content_root),
        ]
    else
        relative = argument.sub(%r{\A/+}, '').sub(%r{/+\z}, '')
        [File.join(content_root, relative, "#{File.basename(relative)}.md")]
    end

    markdown_path = candidates.find { |candidate| File.file?(candidate) }
    abort "Could not find tutorial Markdown for #{argument.inspect}" unless markdown_path

    begin
        relative_markdown = relative_of.call(markdown_path)
    rescue ArgumentError
        abort "Tutorial Markdown must be below #{content_root}: #{markdown_path}"
    end
    if relative_markdown.split(File::SEPARATOR).include?('..')
        abort "Tutorial Markdown must be below #{content_root}: #{markdown_path}"
    end
    if mode != :status && !File.read(File.join(content_root, relative_markdown)).include?(RECIPE)
        abort "Tutorial contains no generated screenshots: #{relative_markdown}"
    end
    relative_markdown
end

# Pages are the Markdown files named like their folder (bif/bif.md), not
# their parts (lehrkraft.md and the like).
pages = Dir.glob(File.join(content_root, '**', '*.md')).sort.filter_map do |path|
    next unless File.basename(path, '.md') == File.basename(File.dirname(path))
    relative = relative_of.call(path)
    next if relative.start_with?('_drafts/')
    relative
end

tutorials = if !args.empty?
    [resolve.call(args.first)]
elsif mode == :status
    pages
else
    pages.select { |page| File.read(File.join(content_root, page)).include?(RECIPE) }
end
abort 'No tutorial contains generated screenshots.' if tutorials.empty?

compose = if system('docker', 'compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker', 'compose']
elsif system('docker-compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker-compose']
else
    abort 'Neither docker compose nor docker-compose is available'
end
ruby_in_container = lambda do |script, *script_args|
    system(*compose, '--project-name', 'workspace', 'exec', '-T', 'ruby',
           'ruby', '-e', script, '--', *script_args, :chdir => repo_root)
end

# Local images and videos a page shows: <img>, <video> and <source> sources
# and Markdown images, without addresses that point elsewhere.
local_media = lambda do |markdown|
    sources = markdown.scan(/<(?:img|video|source)\b[^>]*\bsrc=['"]([^'"]+)['"]/i).flatten +
              markdown.scan(/!\[[^\]]*\]\(([^)\s]+)/).flatten
    sources.map { |src| src.split(/[?#]/, 2).first }
           .reject { |src| src.empty? || src =~ %r{\A(?:[a-z]+:|//|/)}i }
           .uniq
end

color = $stdout.tty?
paint = lambda do |text, code|
    color ? "\e[#{code}m#{text}\e[0m" : text
end

overview = lambda do |relative_paths|
    plan_runner = <<~'INNER_RUBY'
        require 'json'
        require './include/tutorial_screenshots.rb'

        plans = ARGV.to_h do |relative_path|
            begin
                [relative_path, TutorialScreenshots.plan(relative_path)['screenshots']]
            rescue => e
                [relative_path, { 'error' => e.message }]
            end
        end
        puts JSON.generate(plans)
    INNER_RUBY
    with_recipes = relative_paths.select { |page| File.read(File.join(content_root, page)).include?(RECIPE) }
    plans = {}
    unless with_recipes.empty?
        output = IO.popen([*compose, '--project-name', 'workspace', 'exec', '-T', 'ruby', 'ruby', '-e', plan_runner, '--', *with_recipes],
                          :chdir => repo_root, &:read)
        plans = JSON.parse(output.lines.last || '{}') rescue {}
        abort 'Could not read the screenshot state. Are the development services running?' if plans.empty?
    end

    totals = Hash.new(0)
    relative_paths.each do |page|
        markdown = File.read(File.join(content_root, page))
        plan = plans[page] || []
        if plan.is_a?(Hash)
            puts "#{page}  #{paint.call("state unavailable: #{plan['error']}", 31)}"
            next
        end
        generated = plan.map { |item| item['target'] }
        manual = local_media.call(markdown) - generated
        next if generated.empty? && manual.empty?

        counts = plan.map { |item| item['state'] }.tally
        totals[:pages] += 1
        totals[:generated] += generated.size
        totals[:manual] += manual.size
        counts.each { |state, n| totals[state] += n }
        summary = "#{generated.size} generated, #{manual.size} manual"
        badge = if generated.empty? then ''
            elsif counts.keys == ['current'] then paint.call(' ✓', 32)
            else paint.call(" #{generated.size - counts.fetch('current', 0)} out of date", 33)
        end
        puts "#{page}  #{summary}#{badge}"
        plan.each do |item|
            mark = case item['state']
                when 'current' then paint.call('✓', 32)
                when 'missing' then paint.call('✗', 31)
                else paint.call('✗', 33)
            end
            note = item['state'] == 'current' ? '' : "  #{item['state']}"
            puts "    #{mark} #{item['target']}#{note}"
        end
    end
    out_of_date = totals['stale'] + totals['missing']
    state = out_of_date.zero? ? paint.call('all current ✓', 32) : paint.call("#{totals['stale']} stale, #{totals['missing']} missing", 33)
    puts
    puts "Total: #{totals[:generated]} generated (#{state}), #{totals[:manual]} manual, on #{totals[:pages]} page(s)."
end

if mode == :status
    overview.call(tutorials)
    exit 0
end

runner = <<~'INNER_RUBY'
    require './include/tutorial_screenshots.rb'

    force = ARGV.shift == 'force'
    failed = []
    ARGV.each do |relative_path|
        begin
            result = force ? TutorialScreenshots.recreate(relative_path) : TutorialScreenshots.refresh(relative_path)
            puts "#{force ? 'Recreated' : 'Rendered'} #{result['generated'].to_i} of #{result['screenshots'].to_i} generated file(s) for #{relative_path}."
        rescue => e
            failed << relative_path
            puts "#{relative_path}: #{e.message}"
        end
    end
    exit(failed.empty? ? 0 : 1)
INNER_RUBY

ok = Dir.chdir(repo_root) do
    verb = mode == :stale ? 'Rendering out-of-date screenshots' : 'Forcing recreation of generated screenshots'
    puts "#{verb} for #{tutorials.join(', ')}..."
    log_pid = spawn(
        *compose,
        '--project-name', 'workspace',
        'logs', '--follow', '--tail', '0', 'tutorial_screenshots',
        :out => $stdout,
        :err => $stderr,
    )

    begin
        sleep 0.2
        ruby_in_container.call(runner, mode == :stale ? 'stale' : 'force', *tutorials)
    ensure
        Process.kill('TERM', log_pid) rescue nil
        Process.wait(log_pid) rescue nil
    end
end

puts
overview.call(tutorials)
abort 'Screenshot recreation failed for at least one tutorial (see above). Are the development services running?' unless ok
