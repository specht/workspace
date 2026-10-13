#!/usr/bin/env ruby

require 'pathname'

repo_root = File.expand_path('../..', __dir__)
content_root = File.join(repo_root, 'src', 'content')
USAGE = <<~TEXT
    Usage:
      src/scripts/recreate-screenshots.rb TUTORIAL          render every generated image and video of one tutorial
      src/scripts/recreate-screenshots.rb --all             the same for every tutorial with recipes
      src/scripts/recreate-screenshots.rb --stale [TUTORIAL] render only what is out of date (all tutorials, or one)
    TUTORIAL is a folder below src/content (bif, lehrmaterial/lehrmaterial-2d)
    or a Markdown path. Drafts in src/content/_drafts are skipped by --all and --stale.
TEXT

mode = :force
args = ARGV.dup
if args.first == '--all'
    mode = :all
    args.shift
elsif args.first == '--stale'
    mode = :stale
    args.shift
end
abort USAGE if args.size > 1 || (mode == :force && args.empty?) || (mode == :all && !args.empty?)

content_path = Pathname.new(content_root).realpath

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

    markdown_realpath = Pathname.new(markdown_path).realpath
    begin
        relative_markdown = markdown_realpath.relative_path_from(content_path).to_s
    rescue ArgumentError
        abort "Tutorial Markdown must be below #{content_root}: #{markdown_path}"
    end
    if relative_markdown.split(File::SEPARATOR).include?('..')
        abort "Tutorial Markdown must be below #{content_root}: #{markdown_path}"
    end

    unless File.read(markdown_realpath).include?('<!-- tutorial-screenshot')
        abort "Tutorial contains no generated screenshots: #{relative_markdown}"
    end
    relative_markdown
end

tutorials = if args.empty?
    Dir.glob(File.join(content_root, '**', '*.md')).sort.filter_map do |path|
        relative = Pathname.new(path).realpath.relative_path_from(content_path).to_s
        next if relative.start_with?('_drafts/')
        next unless File.read(path).include?('<!-- tutorial-screenshot')

        relative
    end
else
    [resolve.call(args.first)]
end
abort 'No tutorial contains generated screenshots.' if tutorials.empty?

compose = if system('docker', 'compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker', 'compose']
elsif system('docker-compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker-compose']
else
    abort 'Neither docker compose nor docker-compose is available'
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

Dir.chdir(repo_root) do
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
        ok = system(
            *compose,
            '--project-name', 'workspace',
            'exec', '-T', 'ruby',
            'ruby', '-e', runner, '--', mode == :stale ? 'stale' : 'force', *tutorials,
        )
    ensure
        Process.kill('TERM', log_pid) rescue nil
        Process.wait(log_pid) rescue nil
    end

    abort 'Screenshot recreation failed for at least one tutorial (see above). Are the development services running?' unless ok
end
