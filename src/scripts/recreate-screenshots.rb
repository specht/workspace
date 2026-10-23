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

    TUTORIAL is a folder below src/content (bif, reference/workspace)
    or a Markdown path. Drafts in src/content/_drafts are skipped. Every run ends
    with the state of the tutorials it rendered:

      ✓ current   ✗ stale (rendered from an older recipe or setup)   ✗ missing

    The development services must be running. Rendering on page views may be
    paused (status bar of a tutorial page); this script renders either way.

    Pictures a script takes (a .pictures.json beside them, as in the teacher
    guide) are listed after the recipes; --stale and --all take them again
    when the files they come from have changed, TUTORIAL when it is their page.
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
# A page with steps only (no pictures) can still be walked through.
STEP = '<!-- tutorial-step'

require_relative '../ruby/include/content_parts'
# Does a page render pictures? Its own recipes, or a part of a Handbuch page
# it includes whose recipes depend on it (${template}): the generator renders
# that part once for every page (src/tutorial-screenshots/includes.js).
renders = lambda do |text|
    text.include?(RECIPE) || ContentParts.included_parts(text).any? do |included|
        slug, part = included.split('#')
        file = ContentParts.reference_file(File.join(content_root, 'reference'), slug)
        body = file && ContentParts.parts(File.read(file))[part]
        body && body.scan(/<!--\s*tutorial-(?:screenshot|step)\s*\n.*?-->/m).any? { |recipe| recipe.include?('${template') }
    end
end

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
    text = File.read(File.join(content_root, relative_markdown))
    if mode != :status && !renders.call(text) && !(mode == :force && text.include?(STEP))
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
    pages.select { |page| renders.call(File.read(File.join(content_root, page))) }
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
    # Examples in code blocks (<img src="pictures/fuji.jpg">) are no images
    # of the page.
    markdown = markdown.gsub(/^[ \t]*```.*?^[ \t]*```/m, '')
    sources = markdown.scan(/<(?:img|video|source)\b[^>]*\bsrc=['"]([^'"]+)['"]/i).flatten +
              markdown.scan(/!\[[^\]]*\]\(([^)\s]+)/).flatten
    sources.map { |src| src.split(/[?#]/, 2).first }
           .reject { |src| src.empty? || src =~ %r{\A(?:[a-z]+:|//|/)}i }
           .uniq
end

# Pictures a script takes instead of a recipe (the teacher guide's, from a
# Playwright run against the class page): a .pictures.json beside them names
# the script, the files the pictures come from and their hash when they were
# taken. A changed file makes them stale.
require 'digest'
script_pictures = lambda do
    Dir[File.join(content_root, '**', '.pictures.json')].sort.map do |manifest|
        data = JSON.parse(File.read(manifest))
        digest = Digest::SHA256.new
        missing = []
        data['inputs'].sort.each do |file|
            path = File.join(repo_root, file)
            next missing << file unless File.file?(path)

            digest << "#{file}\n" << File.binread(path) << "\n"
        end
        dir = File.dirname(manifest)
        absent = data['pictures'].reject { |name| File.file?(File.join(dir, name)) }
        state = if !absent.empty? || !missing.empty? then 'missing'
            elsif digest.hexdigest == data['hash'] then 'current'
            else 'stale'
        end
        data.merge('dir' => dir, 'page' => Pathname.new(dir).relative_path_from(Pathname.new(content_root)).to_s, 'state' => state)
    end
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
    with_recipes = relative_paths.select { |page| renders.call(File.read(File.join(content_root, page))) }
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
        # A picture borrowed from another page (../reference/workspace/go-live.webp)
        # is generated if that page's manifest lists it.
        page_dir = File.dirname(File.join(content_root, page))
        borrowed = lambda do |src|
            next false unless src.include?('/')
            file = File.expand_path(src, page_dir)
            manifest = File.join(File.dirname(file), '.tutorial-screenshots.json')
            File.exist?(manifest) && (JSON.parse(File.read(manifest))['screenshots'] || {}).key?(File.basename(file))
        end
        by_script = script_pictures.call.select { |item| item['dir'] == page_dir }.flat_map { |item| item['pictures'] }
        manual = local_media.call(markdown).reject { |src| generated.include?(src) || borrowed.call(src) || by_script.include?(src) }
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
    pictures = script_pictures.call
    unless pictures.empty?
        puts
        puts 'Taken by a script:'
        pictures.each do |item|
            mark = item['state'] == 'current' ? paint.call('✓', 32) : paint.call("✗ #{item['state']}", 33)
            puts "  #{item['page']}  #{item['pictures'].size} by #{item['script']} #{mark}"
            puts "      #{item['command']}" unless item['state'] == 'current'
        end
    end
end

# The pictures a script takes, again if they are out of date (or with
# --all always): the script runs in its own environment (e2e for the teacher
# guide) and only when the rendering of recipes went through
run_script_pictures = lambda do |only|
    script_pictures.call.each do |item|
        next if only && !only.include?(item['page'])
        next if mode == :stale && item['state'] == 'current'

        puts "Taking #{item['pictures'].size} picture(s) for #{item['page']} with #{item['script']}..."
        ok = system(item['command'], chdir: repo_root)
        puts "#{item['script']} failed; run it yourself: #{item['command']}" unless ok
    end
end

if mode == :status
    overview.call(tutorials)
    exit 0
end

runner = <<~'INNER_RUBY'
    require './include/tutorial_screenshots.rb'

    force = ARGV.shift == 'force'
    failed = []
    announced = TutorialScreenshots.announce_run(ARGV, force: force)
    puts "Progress: /screenshot-progress on the development site (#{announced['stale']} file(s) on #{announced['pages']} page(s))" if announced
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

run_script_pictures.call(args.empty? ? nil : tutorials.map { |t| File.dirname(t) }) if ok
puts
overview.call(tutorials)
abort 'Screenshot recreation failed for at least one tutorial (see above). Are the development services running?' unless ok
