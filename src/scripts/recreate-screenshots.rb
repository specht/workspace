#!/usr/bin/env ruby

require 'pathname'

repo_root = File.expand_path('../..', __dir__)
content_root = File.join(repo_root, 'src', 'content')
argument = ARGV.shift

if argument.nil? || !ARGV.empty?
    abort 'Usage: src/scripts/recreate-screenshots.rb TUTORIAL'
end

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

content_path = Pathname.new(content_root).realpath
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

compose = if system('docker', 'compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker', 'compose']
elsif system('docker-compose', 'version', :out => File::NULL, :err => File::NULL)
    ['docker-compose']
else
    abort 'Neither docker compose nor docker-compose is available'
end

runner = <<~'INNER_RUBY'
    require './include/tutorial_screenshots.rb'

    relative_path = ARGV.fetch(0)
    result = TutorialScreenshots.recreate(relative_path)
    puts "Recreated #{result['generated'].to_i} of #{result['screenshots'].to_i} generated screenshot(s) for #{relative_path}."
INNER_RUBY

Dir.chdir(repo_root) do
    puts "Forcing recreation of generated screenshots for #{relative_markdown}..."
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
            'ruby', '-e', runner, '--', relative_markdown,
        )
    ensure
        Process.kill('TERM', log_pid) rescue nil
        Process.wait(log_pid) rescue nil
    end

    abort 'Screenshot recreation failed. Are the development services running?' unless ok
end
