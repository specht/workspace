require 'digest'
require 'json'
require 'set'
require 'yaml'
require_relative 'sql_check'
require_relative 'cypher_check'
require_relative 'er_check'
require_relative 'classes_check'
require_relative 'schema_check'
require_relative 'keyboard_check'
require_relative 'story_check'
require_relative 'scene_color'
require_relative 'html_lite'
require_relative 'python_check'
require_relative 'c_check'
require_relative 'java_check'
require_relative 'dart_check'
require_relative 'hint_steps'
require_relative 'ruby_check'
require_relative 'kata_check'
require_relative 'tests_check'
require_relative 'pip_check'
require_relative 'circuit_check'
require_relative 'code_languages'

# Format and validation of an activity's tasks.yaml (see docs/aufgaben.md).
#
# This module only reads and checks task definitions. It knows nothing about
# Neo4j, Workspaces or HTTP, so the content parser, the check engine and the
# tests can all use it.
module TaskSpec
    FORMAT_VERSION = 1

    ID_PATTERN = /\A[a-z0-9][a-z0-9-]{0,47}\z/
    VARIABLE_PATTERN = /\$\{([a-z_.]+)\}/
    VARIABLES = %w[workspace.ip workspace.mac workspace.live_url person.codeword].freeze

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
        'story' => 'M7',
        'submit' => 'M3',
        'git' => 'M3',
        'measure' => 'M6',
        'command' => 'M6',
        'scene' => 'M6',
        'sql' => 'M6',
        'cypher' => 'M7',
        'er' => 'M7',
        'classes' => 'M8',
        'schema' => 'M7',
        'keyboard' => 'M7',
        'python' => 'M7',
        'ruby' => 'M7',
        'javascript' => 'M8',
        'c' => 'M7',
        'java' => 'M8',
        'dart' => 'M8',
        'function' => 'M8',
        'pip' => 'M8',
        'tests' => 'M8',
        'circuit' => 'M8',
    }.freeze

    # Check types that need the network; they are never used in exams.
    NEEDS_NETWORK = %w[measure].to_set.freeze

    TASK_KEYS = (%w[id title prompt check hint solution examples feedback prediction file exam starter level extra] + CodeLanguages::LANGUAGES).freeze
    # The levels of a Kata (docs/plan-katas.md): 1 Einstieg, 2 Übung, 3 Knobelei.
    LEVELS = [1, 2, 3].freeze

    EXTRACTORS = %w[ping_rtt_avg ping_hops ping_reply_country traceroute_hops dns_answers].freeze
    EXTRACTOR_WITH_ARGUMENT = /\Ahttp_header:[A-Za-z0-9-]+\z/

    FILES_EXPECT_KEYS = %w[exists executable format new_files name format_matches_extension max_size contains].freeze
    HTML_EXPECT_KEYS = %w[parses slides first_slide_changed img elements].freeze
    SCENE_MATCHER_KEYS = %w[
        shape name at count min rows size bottom_y rotate_z lattice min_spacing moves orbits
        color label
    ].freeze
    SUBMIT_REQUIRES_KEYS = %w[html_parses scene_loads min_objects].freeze
    GIT_EXPECT_KEYS = %w[has_remote min_commits committed pushed identity].freeze

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
        'story' => %w[problem],
        'git' => %w[problem],
        'reflection' => [],
        'submit' => [],
        'sql' => %w[same_result_as query_matches],
        'cypher' => %w[same_result_as query_matches],
        'er' => %w[problem],
        'classes' => %w[problem],
        'schema' => %w[problem],
        'keyboard' => %w[problem],
        'python' => [],
        'ruby' => [],
        'javascript' => [],
        'c' => [],
        'java' => [],
        'dart' => [],
        'function' => %w[same_as],
        'pip' => [],
        'tests' => [],
        'circuit' => [],
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

    # A task with variants for other programming languages (CodeLanguages):
    # each variant may only replace some fields, and the task must be valid
    # in every language.
    def self.validate_task(task)
        variants = CodeLanguages.variant_languages(task)
        return validate_single_task(task) if variants.empty? && CodeLanguages::LANGUAGES.none? { |l| task.key?(l) }

        errors = []
        CodeLanguages::LANGUAGES.each do |language|
            next unless task.key?(language)

            unless task[language].is_a?(Hash)
                errors << "#{language} must be a mapping with what differs in #{CodeLanguages::NAMES[language]}"
                next
            end
            extra = task[language].keys - CodeLanguages::VARIANT_KEYS
            errors << "#{language}: only #{CodeLanguages::VARIANT_KEYS.join(', ')} can differ, not #{extra.join(', ')}" unless extra.empty?
        end
        return errors unless errors.empty?

        errors.concat(validate_single_task(CodeLanguages.variant(task, nil)))
        variants.each do |language|
            errors.concat(validate_single_task(CodeLanguages.variant(task, language)).map { |e| "#{language}: #{e}" })
        end
        errors
    end

    def self.validate_single_task(task)
        errors = []
        unknown = task.keys - TASK_KEYS
        errors << "unknown keys: #{unknown.join(', ')}" unless unknown.empty?
        errors << 'id must match a-z, 0-9 and -' unless task['id'].is_a?(String) && task['id'] =~ ID_PATTERN
        %w[title prompt].each do |key|
            errors << "#{key} is required" unless nonempty_string?(task[key])
        end
        %w[solution file].each do |key|
            errors << "#{key} must be text" if task.key?(key) && !nonempty_string?(task[key])
        end
        errors.concat(hint_errors(task['hint'])) if task.key?('hint')
        %w[prediction exam extra].each do |key|
            errors << "#{key} must be true or false" if task.key?(key) && ![true, false].include?(task[key])
        end
        errors << "level must be one of #{LEVELS.join(', ')}" if task.key?('level') && !LEVELS.include?(task['level'])
        check = task['check']
        unless check.is_a?(Hash) && AVAILABLE.key?(check['type'])
            errors << "check.type must be one of #{AVAILABLE.keys.join(', ')}"
            return errors
        end
        type = check['type']
        errors.concat(validate_check(type, check).map { |e| "check: #{e}" })
        if task.key?('starter')
            errors << 'starter only works with function, pip and tests' unless %w[function pip tests].include?(type)
            errors << 'starter must be text' unless task['starter'].is_a?(String)
            errors << "starter must be at most #{KataCheck::MAX_CODE} characters" if task['starter'].is_a?(String) && task['starter'].size > KataCheck::MAX_CODE
        end
        if task.key?('examples')
            if type != 'reflection'
                errors << 'examples only work with reflection; other checks have a solution'
            elsif !string_list?(task['examples'])
                errors << 'examples must be a list of answers in Markdown'
            end
        end
        if task['prediction'] && !%w[choice number answer].include?(type)
            errors << 'prediction only works with choice, number and answer'
        end
        if task['exam'] == true && NEEDS_NETWORK.include?(type)
            errors << "#{type} needs the network and cannot be used in exams"
        end
        if task.key?('feedback')
            errors.concat(validate_feedback(type, task['feedback']))
            errors.concat(KataCheck.feedback_errors(task['feedback'])) if type == 'function'
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
            allowed += %w[timeout output placeholder dir]
            if check.key?('dir') && !(check['dir'].is_a?(String) && check['dir'].match?(%r{\A[A-Za-z0-9_][A-Za-z0-9_.-]*(/[A-Za-z0-9_][A-Za-z0-9_.-]*)*/?\z}) && !check['dir'].split('/').include?('..'))
                errors << 'dir must be a folder below /workspace, such as bitstein'
            end
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
                if expect.key?('contains')
                    list = expect['contains']
                    if list.is_a?(Array) && !list.empty?
                        list.each_with_index do |want, i|
                            pattern = want.is_a?(Hash) ? want['pattern'] : want
                            if want.is_a?(Hash) && (want.keys - %w[pattern min after say]).any?
                                errors << "expect.contains[#{i}] knows pattern, min, after and say"
                            end
                            errors.concat(regex_errors(pattern, "expect.contains[#{i}]"))
                        end
                    else
                        errors << 'expect.contains must be a list of regular expressions'
                    end
                end
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
                if expect.key?('elements')
                    if expect['elements'].is_a?(Array) && !expect['elements'].empty?
                        expect['elements'].each do |want|
                            unless want.is_a?(Hash) && want['selector'].is_a?(String) && (want.keys - %w[selector min attr say]).empty?
                                errors << 'expect.elements: each needs a selector (and may have min, attr, say)'
                                next
                            end
                            begin
                                want['selector'].split(/\s+/).each { |step| HtmlLite.parse_step(step) }
                            rescue ArgumentError => e
                                errors << "expect.elements: #{e.message}"
                            end
                        end
                    else
                        errors << 'expect.elements must be a list'
                    end
                end
                if expect.key?('img')
                    img = expect['img']
                    if img.is_a?(Hash)
                        extra = img.keys - %w[min local src_exists alt]
                        errors << "expect.img: unknown keys: #{extra.join(', ')}" unless extra.empty?
                        errors << 'expect.img.src_exists must be case_sensitive or false' if img.key?('src_exists') && !['case_sensitive', false].include?(img['src_exists'])
                        errors << 'expect.img.alt must be required or optional' if img.key?('alt') && !%w[required optional].include?(img['alt'])
                        errors << 'expect.img.local must be a number' if img.key?('local') && !img['local'].is_a?(Integer)
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
            allowed += %w[file after_marker linked_from style_tags expect]
            errors << 'style_tags: true or false' if check.key?('style_tags') && ![true, false].include?(check['style_tags'])
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat(path_errors(check['linked_from'], 'linked_from')) if check.key?('linked_from')
            expect = check['expect']
            unless expect.is_a?(Hash) && !expect.empty? && (expect.keys - %w[parses min_rules declares]).empty?
                errors << 'expect knows parses, min_rules and declares'
            end
            if expect.is_a?(Hash) && expect.key?('declares')
                ok = expect['declares'].is_a?(Array) && !expect['declares'].empty? && expect['declares'].all? do |want|
                    want.is_a?(Hash) && want['property'].is_a?(String) && (want.keys - %w[selector property value media say]).empty?
                end
                errors << 'expect.declares: a list, each with property (and may have selector, value, media, say)' unless ok
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
        when 'story'
            allowed += %w[project expect]
            errors.concat(path_errors(check['project'], 'project'))
            expect = check['expect']
            if !expect.is_a?(Hash) || expect.empty? || !(expect.keys - StoryCheck::KEYS).empty?
                errors << "expect knows #{StoryCheck::KEYS.join(', ')}"
            else
                StoryCheck::MINIMUMS.keys.push('endings').each do |key|
                    errors << "expect.#{key} must be a number of at least 1" if expect.key?(key) && !(expect[key].is_a?(Integer) && expect[key] >= 1)
                end
                %w[all_reachable no_errors own_story].each do |key|
                    errors << "expect.#{key} must be true" if expect.key?(key) && expect[key] != true
                end
            end
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
        when 'cypher'
            allowed += %w[expect_query compare rows sorted_by extra_columns nodes setup]
            queries = check['expect_query'].is_a?(Array) ? check['expect_query'] : [check['expect_query']]
            if queries.empty? || !queries.all? { |q| nonempty_string?(q) }
                errors << 'expect_query must be a query or a list of queries'
            else
                queries.each_with_index do |q, i|
                    _, refusal = CypherCheck.single_read(q)
                    errors << "expect_query[#{i}] must be a single reading query: #{refusal}" if refusal
                end
            end
            errors << 'compare must be same or subset' if check.key?('compare') && !%w[same subset].include?(check['compare'])
            errors << 'rows must be a positive number' if check.key?('rows') && !(check['rows'].is_a?(Integer) && check['rows'] > 0)
            errors << 'rows needs compare: subset' if check.key?('rows') && check['compare'] != 'subset'
            if check.key?('sorted_by') && check['sorted_by'].to_s !~ /\A\s*\d+(\s+(ASC|DESC))?(\s*,\s*\d+(\s+(ASC|DESC))?)*\s*\z/i
                errors << 'sorted_by must name result columns, like "2 DESC" or "1, 3"'
            end
            errors << 'extra_columns must be note, ok or wrong' if check.key?('extra_columns') && !%w[note ok wrong].include?(check['extra_columns'])
            if check.key?('nodes')
                nodes = check['nodes']
                unless nodes.is_a?(Hash) && !nodes.empty? && nodes.all? { |k, v| k.to_s =~ /\A[A-Za-z_][A-Za-z0-9_]*\z/ && (v.nil? || (v.is_a?(Integer) && v >= 0)) }
                    errors << 'nodes must map labels to their node count (or ~)'
                end
            end
            errors << 'setup must be text' if check.key?('setup') && !nonempty_string?(check['setup'])
        when 'python'
            # Calls into the person's Python file, run as the guest (PythonCheck).
            allowed += %w[file cases timeout]
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat(PythonCheck.spec_errors(check))
        when 'ruby'
            # Calls into the person's Ruby file, run as the guest (RubyCheck).
            allowed += %w[file cases timeout]
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat(RubyCheck.spec_errors(check))
        when 'javascript'
            # Calls into the person's JavaScript file, run with node as the guest (JavascriptCheck).
            allowed += %w[file cases timeout]
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat(JavascriptCheck.spec_errors(check))
        when 'function'
            # A Kata: calls functions in the code typed into the card, run in
            # a container of its own (KataCheck).
            allowed += %w[language cases timeout forbid random slow recursive loops gates]
            errors.concat(KataCheck.spec_errors(check))
        when 'tests'
            # A Kata of tests: assertions typed into the card, run against a
            # right and some broken versions (TestsCheck).
            allowed += %w[language function correct mutants]
            errors.concat(TestsCheck.spec_errors(check))
        when 'circuit'
            # A circuit from the Logik-Werkstatt's editor, as a netlist, run
            # round by round on the server (CircuitCheck).
            allowed += CircuitCheck::SPEC_KEYS
            errors.concat(CircuitCheck.spec_errors(check))
        when 'pip'
            # A plan for Pip, run against the world on the server (PipCheck).
            allowed += %w[language commands maps goal max_lines dry buttons]
            errors.concat(PipCheck.spec_errors(check))
        when 'c'
            # Calls functions in the person's C file, compiled and run as the guest (CCheck).
            allowed += %w[file cases timeout tolerance setup]
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat(CCheck.spec_errors(check))
        when 'java', 'dart'
            # Calls into the person's Java classes or Dart file, through a
            # harness written around the calls (JavaCheck, DartCheck).
            allowed += %w[file cases timeout setup]
            errors.concat(path_errors(check['file'], 'file'))
            errors.concat((check['type'] == 'java' ? JavaCheck : DartCheck).spec_errors(check))
        when 'classes'
            # Runs the UML-Werkstatt's core on the class diagram of a .uml file in the Workspace.
            allowed += %w[file classes associations]
            errors.concat(path_errors(check['file'], 'file'))
            errors << 'file must end in .uml' if nonempty_string?(check['file']) && !check['file'].end_with?('.uml')
            errors.concat(ClassesCheck.spec_errors(check))
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
                if type == 'cypher'
                    _, refusal = CypherCheck.single_read(entry['when']['same_result_as'].to_s)
                    errors << "feedback[#{i}].when.same_result_as must be a single reading query: #{refusal}" if refusal
                else
                    _, refusal = SqlCheck.single_select(entry['when']['same_result_as'].to_s)
                    errors << "feedback[#{i}].when.same_result_as must be a single SELECT" if refusal
                end
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
    # A task with variants for other languages has one version for all of
    # them, so that one solved in Ruby counts in Python too; a change to any
    # of their checks changes it. Without variants it is the same as always.
    def self.check_version(task)
        variants = CodeLanguages.variant_languages(task).select { |l| task[l].key?('check') }
        source = variants.empty? ? canonical(task['check']) : canonical([task['check']] + variants.map { |l| [l, task[l]['check']] })
        Digest::SHA256.hexdigest(JSON.generate(source))[0, 16]
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

    # A hint is a text, or a list of steps shown one at a time with a pause
    # between them (HintSteps, docs/aufgaben.md »Tipps in Stufen«).
    def self.hint_errors(hint)
        return [] if nonempty_string?(hint)
        if hint.is_a?(Array) && hint.size.between?(2, HintSteps::MAX_STEPS) && hint.all? { |step| nonempty_string?(step) }
            return []
        end
        ["hint must be text or a list of 2 to #{HintSteps::MAX_STEPS} texts"]
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
