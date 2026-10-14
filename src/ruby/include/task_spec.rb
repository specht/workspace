require 'digest'
require 'json'
require 'set'
require 'yaml'
require_relative 'sql_check'
require_relative 'er_check'
require_relative 'schema_check'
require_relative 'keyboard_check'
require_relative 'scene_color'

# Format and validation of an activity's tasks.yaml (see docs/aufgaben.md).
#
# This module only reads and checks task definitions. It knows nothing about
# Neo4j, Workspaces or HTTP, so the content parser, the check engine and the
# tests can all use it.
module TaskSpec
    FORMAT_VERSION = 1

    ID_PATTERN = /\A[a-z0-9][a-z0-9-]{0,47}\z/
    VARIABLE_PATTERN = /\$\{([a-z_.]+)\}/
    VARIABLES = %w[workspace.ip workspace.live_url person.codeword].freeze

    # Milestone in which each check type becomes available. Types marked
    # 'later' may already be written down; they are validated but not run.
    AVAILABLE = {
        'answer' => 'M2',
        'choice' => 'M2',
        'number' => 'M2',
        'reflection' => 'M2',
        'files' => 'M3',
        'html' => 'M3',
        'css' => 'M3',
        'references' => 'M3',
        'submit' => 'M3',
        'git' => 'M3',
        'measure' => 'M6',
        'command' => 'M6',
        'scene' => 'M6',
        'code' => 'M5',
        'sql' => 'M6',
        'er' => 'M7',
        'schema' => 'M7',
        'keyboard' => 'M7',
    }.freeze

    # Check types that need the network; they are never used in exams.
    NEEDS_NETWORK = %w[measure].to_set.freeze

    TASK_KEYS = %w[id title prompt check hint solution feedback prediction file exam].freeze

    EXTRACTORS = %w[ping_rtt_avg ping_hops ping_reply_country traceroute_hops].freeze
    EXTRACTOR_WITH_ARGUMENT = /\Ahttp_header:[A-Za-z0-9-]+\z/

    FILES_EXPECT_KEYS = %w[exists executable format new_files name format_matches_extension max_size].freeze
    HTML_EXPECT_KEYS = %w[parses slides first_slide_changed img].freeze
    SCENE_MATCHER_KEYS = %w[
        shape name at count min rows size bottom_y rotate_z lattice min_spacing moves orbits
        color label
    ].freeze
    SUBMIT_REQUIRES_KEYS = %w[html_parses scene_loads min_objects].freeze
    GIT_EXPECT_KEYS = %w[has_remote min_commits committed pushed identity].freeze
    CODE_LANGUAGES = %w[ruby python javascript].freeze

    # Keys allowed in feedback[].when, per check type.
    WHEN_KEYS = {
        'answer' => %w[equals matches],
        'choice' => %w[picked],
        'number' => %w[equals between],
        'measure' => %w[field between],
        'command' => %w[output_matches],
        'scene' => %w[found missing source_matches problem],
        'html' => %w[problem],
        'css' => %w[problem],
        'files' => %w[problem],
        'references' => %w[problem],
        'git' => %w[problem],
        'reflection' => [],
        'submit' => [],
        'code' => [],
        'sql' => %w[same_result_as query_matches],
        'er' => %w[problem],
        'schema' => %w[problem],
        'keyboard' => %w[problem],
    }.freeze

    class Error < StandardError
        attr_reader :errors

        def initialize(errors)
            @errors = errors
            super(errors.join("\n"))
        end
    end

    # Loads and validates a tasks.yaml. Raises TaskSpec::Error on problems.
    def self.load_file(path, activity: File.basename(File.dirname(path)))
        data = YAML.safe_load(File.read(path, encoding: 'UTF-8'), permitted_classes: [], aliases: false)
        errors = validate(data, activity: activity)
        raise Error.new(errors.map { |e| "#{path}: #{e}" }) unless errors.empty?
        data
    end

    # Returns a list of error messages; empty means valid.
    def self.validate(data, activity:)
        errors = []
        unless data.is_a?(Hash)
            return ['top level must be a mapping with activity and tasks']
        end
        unknown = data.keys - %w[format activity tasks]
        errors << "unknown top-level keys: #{unknown.join(', ')}" unless unknown.empty?
        if data.key?('format') && data['format'] != FORMAT_VERSION
            errors << "format must be #{FORMAT_VERSION}"
        end
        errors << "activity must be '#{activity}'" unless data['activity'] == activity
        tasks = data['tasks']
        unless tasks.is_a?(Array) && !tasks.empty?
            errors << 'tasks must be a non-empty list'
            return errors
        end
        seen = Set.new
        tasks.each_with_index do |task, index|
            where = "tasks[#{index}]"
            unless task.is_a?(Hash)
                errors << "#{where}: must be a mapping"
                next
            end
            where = "task '#{task['id']}'" if task['id'].is_a?(String)
            errors.concat(validate_task(task).map { |e| "#{where}: #{e}" })
            if task['id'].is_a?(String) && !seen.add?(task['id'])
                errors << "#{where}: duplicate id"
            end
        end
        errors
    end

    def self.validate_task(task)
        errors = []
        unknown = task.keys - TASK_KEYS
        errors << "unknown keys: #{unknown.join(', ')}" unless unknown.empty?
        errors << 'id must match a-z, 0-9 and -' unless task['id'].is_a?(String) && task['id'] =~ ID_PATTERN
        %w[title prompt].each do |key|
            errors << "#{key} is required" unless nonempty_string?(task[key])
        end
        %w[hint solution file].each do |key|
            errors << "#{key} must be text" if task.key?(key) && !nonempty_string?(task[key])
        end
        %w[prediction exam].each do |key|
            errors << "#{key} must be true or false" if task.key?(key) && ![true, false].include?(task[key])
        end
        check = task['check']
        unless check.is_a?(Hash) && AVAILABLE.key?(check['type'])
            errors << "check.type must be one of #{AVAILABLE.keys.join(', ')}"
            return errors
        end
        type = check['type']
        errors.concat(validate_check(type, check).map { |e| "check: #{e}" })
        if task['prediction'] && !%w[choice number answer].include?(type)
            errors << 'prediction only works with choice, number and answer'
        end
        if task['exam'] == true && NEEDS_NETWORK.include?(type)
            errors << "#{type} needs the network and cannot be used in exams"
        end
        if task.key?('feedback')
            errors.concat(validate_feedback(type, task['feedback']))
        end
        errors
    end

    def self.validate_check(type, check)
        errors = []
        allowed = %w[type]
        case type
        when 'answer'
            allowed += %w[expect compare case_sensitive placeholder]
            expect = check['expect']
            values = expect.is_a?(Array) ? expect : [expect]
            errors << 'expect must be text or a list of texts' unless !values.empty? && values.all? { |v| nonempty_string?(v) }
            errors << 'compare must be text, url or regex' if check.key?('compare') && !%w[text url regex].include?(check['compare'])
            values.select { |v| v.is_a?(String) }.each { |v| errors.concat(variable_errors(v)) }
            if check['compare'] == 'regex'
                values.select { |v| v.is_a?(String) }.each { |v| errors.concat(regex_errors(v, 'expect')) }
            end
        when 'choice'
            allowed += %w[options correct shuffle]
            options = check['options']
            unless options.is_a?(Array) && options.size >= 2 && options.all? { |o| nonempty_string?(o) }
                errors << 'options must list at least two texts'
                options = []
            end
            correct = check['correct'].is_a?(Array) ? check['correct'] : [check['correct']]
            unless !correct.empty? && correct.all? { |c| c.is_a?(Integer) && c >= 0 && c < options.size }
                errors << 'correct must be the number of an option, counted from 0'
            end
            errors << 'shuffle must be true or false' if check.key?('shuffle') && ![true, false].include?(check['shuffle'])
        when 'number'
            allowed += %w[expect tolerance unit placeholder]
            errors << 'expect must be a number' unless check['expect'].is_a?(Numeric)
            errors << 'tolerance must be a number >= 0' if check.key?('tolerance') && !(check['tolerance'].is_a?(Numeric) && check['tolerance'] >= 0)
        when 'reflection'
            allowed += %w[min_words placeholder]
            errors << 'min_words must be a positive whole number' if check.key?('min_words') && !(check['min_words'].is_a?(Integer) && check['min_words'] > 0)
        when 'measure'
            allowed += %w[fields compare tolerance tolerance_percent unit]
            fields = check['fields']
            if fields.is_a?(Array) && !fields.empty?
                fields.each_with_index do |field, i|
                    unless field.is_a?(Hash) && nonempty_string?(field['label']) && nonempty_string?(field['run'])
                        errors << "fields[#{i}] needs label and run"
                        next
                    end
                    extra = field.keys - %w[label run extract]
                    errors << "fields[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    unless EXTRACTORS.include?(field['extract']) || field['extract'].to_s =~ EXTRACTOR_WITH_ARGUMENT
                        errors << "fields[#{i}]: extract must be one of #{EXTRACTORS.join(', ')} or http_header:<Name>"
                    end
                end
            else
                errors << 'fields must be a non-empty list'
            end
            errors << 'compare must be number or text' if check.key?('compare') && !%w[number text].include?(check['compare'])
            %w[tolerance tolerance_percent].each do |key|
                errors << "#{key} must be a number >= 0" if check.key?(key) && !(check[key].is_a?(Numeric) && check[key] >= 0)
            end
        when 'command'
            allowed += %w[timeout output placeholder]
            output = check['output']
            if output.is_a?(Hash) && !output.empty?
                extra = output.keys - %w[lines matching not_matching]
                errors << "output: unknown keys: #{extra.join(', ')}" unless extra.empty?
                errors << 'output.lines must be a positive whole number' if output.key?('lines') && !(output['lines'].is_a?(Integer) && output['lines'] > 0)
                %w[matching not_matching].each { |k| errors.concat(regex_errors(output[k], "output.#{k}")) if output.key?(k) }
            else
                errors << 'output must say what the output looks like (lines, matching, not_matching)'
            end
            errors << 'timeout must be 1 to 30 seconds' if check.key?('timeout') && !(check['timeout'].is_a?(Integer) && check['timeout'].between?(1, 30))
        when 'files'
            allowed += %w[path template expect show]
            errors.concat(path_errors(check['path'], 'path'))
            errors << 'template must be a list of file names' if check.key?('template') && !string_list?(check['template'])
            errors << 'show must be listing or none' if check.key?('show') && !%w[listing none].include?(check['show'])
            expect = check['expect']
            if expect.is_a?(Hash) && !expect.empty?
                extra = expect.keys - FILES_EXPECT_KEYS
                errors << "expect: unknown keys: #{extra.join(', ')}" unless extra.empty?
                errors.concat(regex_errors(expect['name'], 'expect.name')) if expect.key?('name')
                errors << 'expect.max_size must look like 500kB or 2MB' if expect.key?('max_size') && parse_size(expect['max_size']).nil?
                if expect.key?('new_files') && !(expect['new_files'].is_a?(Hash) && expect['new_files']['min'].is_a?(Integer))
                    errors << 'expect.new_files must be { min: <number> }'
                end
            else
                errors << 'expect must say what has to be true'
            end
        when 'html'
            allowed += %w[file expect warn]
            errors.concat(path_errors(check['file'], 'file'))
            expect = check['expect']
            if expect.is_a?(Hash) && !expect.empty?
                extra = expect.keys - HTML_EXPECT_KEYS
                errors << "expect: unknown keys: #{extra.join(', ')}" unless extra.empty?
                if expect.key?('slides')
                    slides = expect['slides']
                    if slides.is_a?(Hash)
                        extra = slides.keys - %w[selector min_own each_has template_titles]
                        errors << "expect.slides: unknown keys: #{extra.join(', ')}" unless extra.empty?
                        errors << 'expect.slides.template_titles must be a list of texts' if slides.key?('template_titles') && !string_list?(slides['template_titles'])
                    else
                        errors << 'expect.slides must be a mapping'
                    end
                end
                if expect.key?('first_slide_changed') && !(expect['first_slide_changed'].is_a?(Hash) && string_list?(expect['first_slide_changed']['placeholders']))
                    errors << 'expect.first_slide_changed must list placeholders'
                end
                if expect.key?('img')
                    img = expect['img']
                    if img.is_a?(Hash)
                        extra = img.keys - %w[min src_exists alt]
                        errors << "expect.img: unknown keys: #{extra.join(', ')}" unless extra.empty?
                        errors << 'expect.img.src_exists must be case_sensitive or false' if img.key?('src_exists') && !['case_sensitive', false].include?(img['src_exists'])
                        errors << 'expect.img.alt must be required or optional' if img.key?('alt') && !%w[required optional].include?(img['alt'])
                    else
                        errors << 'expect.img must be a mapping'
                    end
                end
            else
                errors << 'expect must say what has to be true'
            end
            if check.key?('warn')
                warn = check['warn']
                unless warn.is_a?(Hash) && (warn.keys - %w[words_per_slide]).empty? && warn.values.all? { |v| v.is_a?(Integer) && v > 0 }
                    errors << 'warn knows only words_per_slide: <number>'
                end
            end
        when 'css'
            allowed += %w[file after_marker expect]
            errors.concat(path_errors(check['file'], 'file'))
            expect = check['expect']
            unless expect.is_a?(Hash) && !expect.empty? && (expect.keys - %w[parses min_rules]).empty?
                errors << 'expect knows parses and min_rules'
            end
        when 'references'
            allowed += %w[root scan expect]
            errors.concat(path_errors(check['root'], 'root'))
            errors << 'scan must be a list of files' unless string_list?(check['scan'])
            expect = check['expect']
            unless expect.is_a?(Hash) && (expect.keys - %w[external local_exist]).empty? && !expect.empty?
                errors << 'expect knows external and local_exist'
            end
            errors << 'expect.external must be none or allowed' if expect.is_a?(Hash) && expect.key?('external') && !%w[none allowed].include?(expect['external'])
        when 'scene'
            allowed += %w[file at_times ignore expect source background]
            errors.concat(color_errors(check['background'], 'background')) if check.key?('background')
            errors.concat(path_errors(check['file'], 'file'))
            errors << 'at_times must be a list of numbers' if check.key?('at_times') && !(check['at_times'].is_a?(Array) && !check['at_times'].empty? && check['at_times'].all? { |t| t.is_a?(Numeric) })
            errors << 'ignore must be a list of shapes' if check.key?('ignore') && !string_list?(check['ignore'])
            matchers = check['expect'].is_a?(Hash) ? [check['expect']] : check['expect']
            if matchers.is_a?(Array) && !matchers.empty?
                matchers.each_with_index do |m, i|
                    unless m.is_a?(Hash) && !m.empty?
                        errors << "expect[#{i}] must be a mapping"
                        next
                    end
                    extra = m.keys - SCENE_MATCHER_KEYS
                    errors << "expect[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    %w[at size lattice].each do |k|
                        errors << "expect[#{i}].#{k} must be three numbers" if m.key?(k) && !(m[k].is_a?(Array) && m[k].size == 3 && m[k].all? { |v| v.is_a?(Numeric) })
                    end
                    errors.concat(color_errors(m['color'], "expect[#{i}].color")) if m.key?('color')
                    errors << "expect[#{i}].label must be text" if m.key?('label') && !nonempty_string?(m['label'])
                end
                labels = matchers.select { |m| m.is_a?(Hash) && m['label'] }.map { |m| m['label'] }
                errors << 'labels must be different' if labels.uniq.size != labels.size
            else
                errors << 'expect must list what the scene contains'
            end
            if check.key?('source')
                source = check['source']
                unless source.is_a?(Hash) && (source.keys - %w[contains]).empty? && string_list?(source['contains'])
                    errors << 'source knows contains: [words]'
                end
            end
        when 'submit'
            allowed += %w[paths pick exclude max_size gallery preview requires]
            errors << 'preview knows anaglyph' if check.key?('preview') && check['preview'] != 'anaglyph'
            errors << 'preview: anaglyph needs pick: …/*.ini' if check['preview'] == 'anaglyph' && !check['pick'].to_s.end_with?('.ini')
            errors.concat(path_errors(check['pick'], 'pick')) if check.key?('pick')
            errors << 'use either paths or pick' if check.key?('paths') && check.key?('pick')
            errors << 'paths must be a list' if check.key?('paths') && !string_list?(check['paths'])
            (check['paths'] || []).each { |p| errors.concat(path_errors(p, 'paths')) } if string_list?(check['paths'])
            errors << 'exclude must be a list' if check.key?('exclude') && !string_list?(check['exclude'])
            errors << 'max_size must look like 500kB or 20MB' if check.key?('max_size') && parse_size(check['max_size']).nil?
            errors << 'gallery must be true or false' if check.key?('gallery') && ![true, false].include?(check['gallery'])
            if check.key?('requires')
                requires = check['requires']
                unless requires.is_a?(Hash) && (requires.keys - SUBMIT_REQUIRES_KEYS).empty?
                    errors << "requires knows #{SUBMIT_REQUIRES_KEYS.join(', ')}"
                end
            end
        when 'git'
            # Reads the state of a repository in the student's Workspace
            # (git status, run inside the container). Never changes it.
            # Without repo it only asks who Git thinks you are (identity).
            allowed += %w[repo expect]
            expect = check['expect']
            identity_only = expect.is_a?(Hash) && expect.keys == ['identity']
            errors.concat(path_errors(check['repo'], 'repo')) unless identity_only && !check.key?('repo')
            unless expect.is_a?(Hash) && !expect.empty? && (expect.keys - GIT_EXPECT_KEYS).empty?
                errors << "expect knows #{GIT_EXPECT_KEYS.join(', ')}"
            end
            if expect.is_a?(Hash)
                (expect.keys & %w[committed pushed has_remote identity]).each do |k|
                    errors << "expect.#{k} must be true" unless expect[k] == true
                end
                errors << 'expect.min_commits must be a positive whole number' if expect.key?('min_commits') && !(expect['min_commits'].is_a?(Integer) && expect['min_commits'] > 0)
            end
        when 'code'
            allowed += %w[codebite languages]
            errors << 'codebite must name a Code Bites task' unless check['codebite'].to_s =~ /\A[a-z0-9_-]+\z/
            if check.key?('languages') && !(string_list?(check['languages']) && (check['languages'] - CODE_LANGUAGES).empty?)
                errors << "languages must be a list of #{CODE_LANGUAGES.join(', ')}"
            end
        when 'sql'
            allowed += %w[expect_query compare rows sorted_by extra_columns tables setup]
            queries = check['expect_query'].is_a?(Array) ? check['expect_query'] : [check['expect_query']]
            if queries.empty? || !queries.all? { |q| nonempty_string?(q) }
                errors << 'expect_query must be a query or a list of queries'
            else
                queries.each_with_index do |q, i|
                    _, refusal = SqlCheck.single_select(q)
                    errors << "expect_query[#{i}] must be a single SELECT: #{refusal}" if refusal
                end
            end
            errors << 'compare must be same or subset' if check.key?('compare') && !%w[same subset].include?(check['compare'])
            errors << 'rows must be a positive number' if check.key?('rows') && !(check['rows'].is_a?(Integer) && check['rows'] > 0)
            errors << 'rows needs compare: subset' if check.key?('rows') && check['compare'] != 'subset'
            if check.key?('sorted_by') && check['sorted_by'].to_s !~ /\A\s*\d+(\s+(ASC|DESC))?(\s*,\s*\d+(\s+(ASC|DESC))?)*\s*\z/i
                errors << 'sorted_by must name result columns, like "2 DESC" or "1, 3"'
            end
            errors << 'extra_columns must be note, ok or wrong' if check.key?('extra_columns') && !%w[note ok wrong].include?(check['extra_columns'])
            if check.key?('tables')
                tables = check['tables']
                unless tables.is_a?(Hash) && !tables.empty? && tables.all? { |k, v| k.to_s =~ /\A[A-Za-z_][A-Za-z0-9_]*\z/ && (v.nil? || (v.is_a?(Integer) && v >= 0)) }
                    errors << 'tables must map table names to their row count (or ~)'
                end
            end
            errors << 'setup must be text' if check.key?('setup') && !nonempty_string?(check['setup'])
        when 'er'
            # Runs the ER-Werkstatt core on a .er file in the Workspace.
            allowed += %w[file entities relationships tables]
            errors.concat(path_errors(check['file'], 'file'))
            errors << 'file must end in .er' if nonempty_string?(check['file']) && !check['file'].end_with?('.er')
            errors.concat(ErCheck.spec_errors(check))
        when 'schema'
            # Reads information_schema of the person's database, as them.
            allowed += %w[tables setup]
            errors.concat(SchemaCheck.spec_errors(check))
        when 'keyboard'
            # Reads the Keyboard-Tutorial's state file in /workspace.
            allowed += %w[section]
            errors.concat(KeyboardCheck.spec_errors(check))
        end
        extra = check.keys - allowed
        errors << "unknown keys for #{type}: #{extra.join(', ')}" unless extra.empty?
        errors
    end

    def self.validate_feedback(type, feedback)
        errors = []
        unless feedback.is_a?(Array) && !feedback.empty?
            return ['feedback must be a list of { when, say }']
        end
        feedback.each_with_index do |entry, i|
            unless entry.is_a?(Hash) && entry['when'].is_a?(Hash) && nonempty_string?(entry['say']) && (entry.keys - %w[when say]).empty?
                errors << "feedback[#{i}] must be { when: {...}, say: text }"
                next
            end
            extra = entry['when'].keys - WHEN_KEYS.fetch(type, [])
            if entry['when'].key?('same_result_as')
                _, refusal = SqlCheck.single_select(entry['when']['same_result_as'].to_s)
                errors << "feedback[#{i}].when.same_result_as must be a single SELECT" if refusal
            end
            errors << "feedback[#{i}].when: #{type} knows #{WHEN_KEYS.fetch(type, []).join(', ').then { |s| s.empty? ? 'no conditions' : s }}" unless extra.empty?
            %w[matches output_matches source_matches query_matches].each do |k|
                errors.concat(regex_errors(entry['when'][k], "feedback[#{i}].when.#{k}")) if entry['when'].key?(k)
            end
        end
        errors
    end

    # Version of a task's grading: changes when anything in check changes,
    # not when the wording of title, prompt or hints changes.
    def self.check_version(task)
        Digest::SHA256.hexdigest(JSON.generate(canonical(task['check'])))[0, 16]
    end

    def self.canonical(value)
        case value
        when Hash then value.keys.sort.map { |k| [k, canonical(value[k])] }
        when Array then value.map { |v| canonical(v) }
        else value
        end
    end

    def self.parse_size(value)
        return value if value.is_a?(Integer) && value > 0
        m = value.to_s.strip.match(/\A(\d+(?:\.\d+)?)\s*(B|kB|MB)\z/)
        return nil unless m
        (m[1].to_f * { 'B' => 1, 'kB' => 1000, 'MB' => 1_000_000 }[m[2]]).round
    end

    def self.nonempty_string?(value)
        value.is_a?(String) && !value.strip.empty?
    end

    def self.string_list?(value)
        value.is_a?(Array) && !value.empty? && value.all? { |v| nonempty_string?(v) }
    end

    # A colour family (yellow) or a list of them ([yellow, orange]).
    def self.color_errors(value, name)
        families = value.is_a?(Array) ? value : [value]
        return [] if !families.empty? && families.all? { |f| SceneColor::FAMILIES.include?(f) }

        ["#{name} must be one or more of #{SceneColor::FAMILIES.join(', ')}"]
    end

    def self.path_errors(value, name)
        return ["#{name} is required"] unless nonempty_string?(value)
        return ["#{name} must be relative to /workspace"] if value.start_with?('/') || value.start_with?('~')
        return ["#{name} must not leave /workspace"] if value.split('/').include?('..')
        []
    end

    def self.variable_errors(text)
        text.scan(VARIABLE_PATTERN).flatten.reject { |name| VARIABLES.include?(name) }.map do |name|
            "unknown variable ${#{name}}; known: #{VARIABLES.join(', ')}"
        end
    end

    def self.regex_errors(value, name)
        return ["#{name} must be a regular expression"] unless nonempty_string?(value)
        Regexp.new(value.gsub(VARIABLE_PATTERN, 'x'))
        []
    rescue RegexpError => e
        ["#{name} is not a valid regular expression: #{e.message}"]
    end
end
