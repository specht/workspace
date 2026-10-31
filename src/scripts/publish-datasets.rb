#!/usr/bin/env ruby
# Publishes the classroom datasets to specht/workspace-files: copies what the
# collectors built into a clone of that repository, writes the overview
# README.md there with the tables and rows of every dataset, rebuilds the
# tables of the Query Stepper here, and commits (and pushes) there.
#
#   src/scripts/publish-datasets.rb --repo ~/workspace-files countries          # copy, then look
#   src/scripts/publish-datasets.rb --repo ~/workspace-files countries --push   # and publish
#   src/scripts/publish-datasets.rb --repo ~/workspace-files                    # only the overview
#   src/scripts/publish-datasets.rb --patch /tmp/datasets.patch countries      # a patch for git am
#
# A dataset comes from its collector's default output directory, or from
# --from NAME=DIR. Tasks name the row counts they expect (tables: in
# tasks.yaml): a build whose counts differ is refused, unless --force,
# since every check of those tasks would fail until the tasks follow.
require 'fileutils'
require 'json'
require 'open3'
require 'optparse'
require 'tmpdir'
require 'yaml'

ROOT = File.expand_path('../..', __dir__)
REMOTE = 'https://github.com/specht/workspace-files.git'

DATASETS = {
    'countries' => {
        dir: 'country-archive', from: 'src/scripts/collect-countries/country-archive',
        title: 'Countries of the world',
        about: 'The continents, the 197 countries of the world, every city with at least 100,000 inhabitants and every capital, the land borders between the countries (each in both directions), their currencies and official languages. `countries-basic.sql` has only continents, countries and cities, for the first steps.',
        source: '[Wikidata](https://www.wikidata.org/), CC0 1.0',
        used: '*Einführung in SQL*, the Query Stepper',
        stepper: { 'countries' => %w[countries.sql], 'countries-basic' => %w[countries-basic.sql] }
    },
    'movies' => {
        dir: 'movie-archive', from: 'src/scripts/collect-movies/movie-archive',
        title: 'Movies',
        about: 'Well-known feature films, their genres and the people who made them: cast, direction, writing, music, production, camera and editing. Also an invented video shop: copies of some of the films, customers and their loans.',
        source: '[Wikidata](https://www.wikidata.org/), CC0 1.0; IMDb votes only to choose the films; the video shop is made up',
        used: '*Filmdatenbank* (MySQL and Neo4j), the Query Stepper',
        stepper: { 'movies' => %w[movies-schema.sql movies-data-films.sql movies-data-crew.sql],
                   'movies-rental' => %w[movies-schema.sql movies-data-films.sql movies-data-rental.sql] },
        # the names before October 2026, for students halfway through the Filmdatenbank
        old_files: %w[mysql.sql mysql-schema.sql mysql-data.sql mysql-data-movies.sql mysql-data-crew.sql neo4j.dump]
    },
    'streets' => {
        dir: 'street-archive', from: 'src/scripts/collect-streets/street-archive',
        title: 'Streets of Berlin',
        about: 'Every street, path and stair of Berlin and of each of its twelve Bezirke, one way per line with its kind, one-way flag, name and points, compressed; next to each the public places to start from and go to: sights, squares and every school. Never addresses.',
        source: '[OpenStreetMap](https://www.openstreetmap.org/copyright), © OpenStreetMap contributors, ODbL 1.0',
        used: '*Ein Routenplaner für Berlin* (`server.rb` of the template loads a Bezirk from here)',
        # whole files, not SQL: only what the activity downloads
        files: /\A(README\.md|.+\.tsv\.gz|.+-places\.tsv)\z/
    },
    'music' => {
        dir: 'music-archive', from: 'src/scripts/collect-music/discogs',
        title: 'Music',
        about: 'The albums of a chosen list of artists and bands, with their tracks, genres and who plays in which band.',
        source: '[Discogs data dumps](https://data.discogs.com/), CC0 1.0',
        used: 'no activity yet',
        stepper: { 'music' => %w[music-schema.sql] }
    }
}.freeze

# Files beside the datasets, for the overview
OTHER_FILES = {
    'terra1.sql' => 'the old country database of the *Einführung in SQL* (until October 2026), kept for old links',
    'users.sql' => 'a table of users with passwords for *Bobby Tables* (SQL injection)',
    'working-with-files.tar.gz' => 'the files to explore in *Arbeiten mit Dateien*',
    'snake.c' => 'a small game in C, not used by an activity at the moment',
    'raycaster' => '`palette.rb` (a palette of 256 colours, the shade table and the textures as `palette.h`, from seven tiles drawn in the 2D Game Studio) and `textures.png` (the seven tiles of the raycaster) for *Raycasting in C*; copied from `src/content/raycaster/werkzeug/`'
}.freeze

# What every dataset has, by its name
def required(name)
    return ['README.md', 'berlin.tsv.gz', 'steglitz-zehlendorf.tsv.gz', 'steglitz-zehlendorf-places.tsv'] if name == 'streets'

    ['README.md', "#{name}.sql", "#{name}-schema.sql", "#{name}-data.sql", "#{name}-neo4j.dump"] +
        (name == 'countries' ? ['countries-basic.sql'] : [])
end

class Publisher
    def initialize(names:, repo:, from:, commit:, push:, patch:, force:)
        @names = names
        @repo = repo
        @from = from
        @commit = commit || push || patch
        @push = push
        @patch = patch
        @force = force
    end

    def run
        sources = @names.to_h { |name| [name, source(name)] }
        if @patch
            Dir.mktmpdir('workspace-files') do |dir|
                git(dir, 'clone', '--quiet', @repo || REMOTE, dir)
                publish(dir, sources)
            end
        else
            abort 'Say which clone of specht/workspace-files to use: --repo DIR (or WORKSPACE_FILES=DIR).' unless @repo
            dirty = git(@repo, 'status', '--porcelain')
            abort "#{@repo} has changes of its own; commit or stash them first:\n#{dirty}" unless dirty.empty?
            publish(@repo, sources)
        end
    end

    def source(name)
        dataset = DATASETS.fetch(name) { abort "Unknown dataset #{name}; known: #{DATASETS.keys.join(', ')}" }
        dir = File.expand_path(@from[name] || dataset[:from], ROOT)
        missing = required(name).reject { |file| File.file?(File.join(dir, file)) }
        abort "#{name}: #{dir} lacks #{missing.join(', ')}; run the collector first (or --from #{name}=DIR)" unless missing.empty?
        dir
    end

    def publish(repo, sources)
        # the new builds, and what workspace-files has of the others
        rows = DATASETS.to_h { |name, d| [name, row_counts(File.join(sources[name] || File.join(repo, d[:dir]), "#{name}.sql"))] }
        check_tasks(rows, sources.keys)
        sources.each { |name, dir| copy(dir, File.join(repo, DATASETS[name][:dir]), DATASETS[name][:old_files] || [], DATASETS[name][:files]) }
        sources.each_key { |name| rebuild_stepper(name, File.join(repo, DATASETS[name][:dir])) if DATASETS[name][:stepper] }
        File.write(File.join(repo, 'README.md'), overview(repo, rows))
        git(repo, 'add', '--all')
        changes = git(repo, 'status', '--short')
        if changes.empty?
            puts 'Nothing to publish: workspace-files already has these files.'
            return
        end
        puts changes
        unless @commit
            puts "\nCopied into #{repo}. Look, then publish with --push (or commit there yourself)."
            return
        end

        git(repo, 'commit', '--quiet', '-m', message(sources.keys))
        if @patch
            patch = git(repo, 'format-patch', '-1', '--stdout')
            File.write(@patch, patch)
            puts "\nWrote #{@patch}; apply it with git am in a clone of workspace-files."
        elsif @push
            git(repo, 'push', '--quiet')
            puts "\nPublished to #{git(repo, 'remote', 'get-url', 'origin').strip}."
        else
            puts "\nCommitted in #{repo}; push it from there."
        end
    end

    # The files of a dataset, without the collector's caches (dot files), and
    # for a dataset of whole files only those it names; what the new build no
    # longer has goes, but for the old names kept for old links.
    def copy(from, to, keep, only = nil)
        FileUtils.mkdir_p(to)
        files = Dir.children(from).reject { |f| f.start_with?('.') }.select { |f| File.file?(File.join(from, f)) }
        files = files.grep(only) if only
        Dir.children(to).each do |f|
            next if f.start_with?('.') || files.include?(f) || keep.include?(f) || !File.file?(File.join(to, f))

            FileUtils.rm(File.join(to, f))
        end
        files.each { |f| FileUtils.cp(File.join(from, f), File.join(to, f)) }
    end

    # { table => rows } from the INSERT statements of NAME.sql, one row a line
    def row_counts(path)
        return {} unless File.file?(path)

        counts = Hash.new(0)
        table = nil
        File.foreach(path) do |line|
            if (m = line.match(/\AINSERT INTO `(\w+)`/))
                table = m[1]
                counts[table] += 0
            elsif line.start_with?(';')
                table = nil
            elsif table && line.start_with?('  (')
                counts[table] += 1
            end
        end
        counts
    end

    # Every count a task expects (tables: { city: 4748 }) against the data of
    # the datasets the task's activity loads
    def check_tasks(rows, published)
        problems = []
        Dir[File.join(ROOT, 'src/content/*/tasks.yaml')].sort.each do |path|
            activity = File.basename(File.dirname(path))
            page = File.join(File.dirname(path), "#{activity}.md")
            used = DATASETS.select { |_, d| File.file?(page) && File.read(page).include?("workspace-files/raw/main/#{d[:dir]}/") }.keys
            ((YAML.load_file(path) || {})['tasks'] || []).each do |task|
                tables = (task['check'] || {})['tables']
                next unless tables.is_a?(Hash)

                counts = tables.select { |_, n| n.is_a?(Integer) }
                next if counts.empty?

                (used & published).each do |name|
                    next unless counts.keys.all? { |table| rows[name].key?(table) }

                    counts.each do |table, n|
                        problems << "#{activity}/#{task['id']}: expects #{n} rows in #{table}, the build has #{rows[name][table]}" if rows[name][table] != n
                    end
                end
            end
        end
        return if problems.empty?

        warn problems.uniq.join("\n")
        warn 'Bring tasks.yaml (and the numbers in its solutions) to the new build, render the screenshots of the page, and publish again.'
        abort 'Refused; --force publishes all the same.' unless @force
    end

    def rebuild_stepper(name, dir)
        DATASETS[name][:stepper].each do |json, names|
            files = names.map { |f| File.join(dir, f) }
            out, status = Open3.capture2e('ruby', File.join(ROOT, 'src/scripts/query-stepper-data.rb'), json, *files)
            abort "query-stepper-data.rb #{json} failed:\n#{out}" unless status.success?
            puts "Query Stepper: #{out.lines.last.strip} (commit it in specht/workspace)"
        end
    end

    def generated(dir)
        File.file?(File.join(dir, 'README.md')) && File.read(File.join(dir, 'README.md'))[/^Generated: (\S+)/, 1].to_s[0, 10]
    end

    def old_note(dir, dataset)
        old = (dataset[:old_files] || []).select { |f| File.file?(File.join(dir, f)) }
        return '' if old.empty?

        "\nStill there under the names before October 2026, for old links, until the end of the\nschool year: #{old.map { |f| "`#{f}`" }.join(', ')}."
    end

    def overview(repo, rows)
        sections = DATASETS.filter_map do |name, d|
            dir = File.join(repo, d[:dir])
            next unless File.directory?(dir)

            thousands = ->(n) { n.to_s.reverse.scan(/\d{1,3}/).join(',').reverse }
            contents = if d[:files]
                files = Dir.children(dir).grep(d[:files]).reject { |f| f == 'README.md' }.sort
                ['| File | Size |', '| --- | ---: |'] +
                    files.map { |f| "| `#{f}` | #{thousands.(File.size(File.join(dir, f)) / 1024)} KB |" }
            else
                ['| Table | Rows |', '| --- | ---: |'] +
                    rows[name].map { |table, n| "| `#{table}` | #{thousands.(n)} |" }
            end
            <<~SECTION
                ## #{d[:title]}: [`#{d[:dir]}/`](#{d[:dir]}/)

                #{d[:about]}

                #{contents.join("\n")}

                Source: #{d[:source]}. Built #{generated(dir) || 'at an unknown date'}. Used by: #{d[:used]}.
                What each column means: [`#{d[:dir]}/README.md`](#{d[:dir]}/README.md).#{old_note(dir, d)}
            SECTION
        end
        known = DATASETS.values.map { |d| d[:dir] } + %w[README.md]
        others = Dir.children(repo).reject { |f| f.start_with?('.') || known.include?(f) }.sort
        <<~README
            # Files for the Hackschule Workspace

            Files that students download with `wget` in the activities of the
            [Hackschule Workspace](https://github.com/specht/workspace), above all the
            classroom datasets. Each SQL dataset folder has the same files, named after the
            dataset: `countries.sql` (tables and data), `countries-schema.sql` (tables only),
            `countries-data.sql` (data only, for tables built in class),
            `countries-data-*.sql` (the data in parts), `countries-neo4j.dump` (for
            `neo4j_bolt load`), the source data as JSON Lines (`*.txt`) and a `README.md`
            with what each column means. The datasets are built by the scripts
            in `src/scripts/collect-*` of the Workspace and published with
            `src/scripts/publish-datasets.rb`, which also writes this file.

            #{sections.join("\n")}
            ## Other files

            #{others.map { |f| "- `#{f}`#{OTHER_FILES[f] ? ": #{OTHER_FILES[f]}" : ''}" }.join("\n")}
        README
    end

    def message(names)
        subject = names.empty? ? 'Overview of the datasets' : "Publish #{names.join(', ')}"
        "#{subject}\n\nFrom src/scripts/publish-datasets.rb in specht/workspace."
    end

    def git(dir, *args)
        out, status = Open3.capture2e('git', '-C', dir, *args)
        abort "git #{args.join(' ')} in #{dir} failed:\n#{out}" unless status.success?
        out
    end
end

options = { repo: ENV['WORKSPACE_FILES'], from: {}, commit: false, push: false, patch: nil, force: false }
parser = OptionParser.new do |opts|
    opts.banner = "Usage: publish-datasets.rb [options] [#{DATASETS.keys.join('|')} ...]"
    opts.on('--repo DIR', 'Clone of specht/workspace-files (default: $WORKSPACE_FILES)') { |dir| options[:repo] = File.expand_path(dir) }
    opts.on('--from NAME=DIR', 'Take a dataset from DIR instead of its collector\'s output') do |value|
        name, dir = value.split('=', 2)
        options[:from][name] = File.expand_path(dir)
    end
    opts.on('--commit', 'Commit in the clone') { options[:commit] = true }
    opts.on('--push', 'Commit and push') { options[:push] = true }
    opts.on('--patch FILE', 'Work in a fresh clone (of --repo, or GitHub) and write a patch for git am') { |f| options[:patch] = File.expand_path(f) }
    opts.on('--force', 'Publish although tasks expect other row counts') { options[:force] = true }
end

if $PROGRAM_NAME == __FILE__
    parser.parse!
    Publisher.new(names: ARGV.uniq, **options).run
end
