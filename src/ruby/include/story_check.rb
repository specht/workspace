require 'digest'
require 'json'

# Check type story (docs/aufgaben.md): a BIF story, judged from the analysis
# that BIF itself publishes while the student writes (.story-tools/
# analysis.json, written by the BIF Authoring Tools in the student's
# Workspace). The server only reads that JSON and the story's files, to see
# whether the analysis is current; it never runs the story or its scripts.
module StoryCheck
    ANALYSIS = '.story-tools/analysis.json'

    # What expect may ask for, with the summary field it counts and the
    # words for the feedback.
    MINIMUMS = {
        'pages' => ['pages', 'Seiten', 'Seite'],
        'choices' => ['pageChoices', 'Entscheidungen, die zu einer anderen Seite führen', 'Entscheidung, die zu einer anderen Seite führt'],
        'local_choices' => ['localChoices', 'lokale Entscheidungen (mit `(.)`)', 'lokale Entscheidung (mit `(.)`)'],
        'conditions' => ['conditions', 'Bedingungen (`condition="…"`)', 'Bedingung (`condition="…"`)'],
        'scripts' => ['scripts', 'Skripte (`<script>`)', 'Skript (`<script>`)'],
        'images' => ['images', 'Bilder', 'Bild'],
    }.freeze
    KEYS = (MINIMUMS.keys + %w[endings all_reachable no_errors own_story]).freeze
    STARTER = 'pages-starter'

    # Reachable pages without a choice that leads to another page: there the
    # story ends, and BIF offers to play again. Every such choice is an edge.
    def self.endings(analysis)
        unreachable = (analysis['diagnostics'] || []).select { |d| d['code'] == 'unreachable-page' }.map { |d| d['file'] }
        leaving = (analysis['edges'] || []).map { |edge| edge['sourceNodeId'] }
        (analysis['nodes'] || []).select do |node|
            node['kind'] == 'page' && node['reachable'] != false && !unreachable.include?(node['path']) &&
                !leaving.include?(node['nodeId'])
        end
    end

    # The summary with every count MINIMUMS asks for: BIF leaves scripts,
    # conditions and images out of it, they are summed from the pages.
    def self.totals(analysis)
        summary = (analysis['summary'] || {}).dup
        MINIMUMS.each_value do |(field, _, _)|
            next if summary.key?(field)
            summary[field] = (analysis['nodes'] || []).sum { |node| node.dig('counts', field).to_i }
        end
        summary
    end

    # Story files whose content differs from what the analysis read: the
    # analysis is older than the story (the Workspace was not running).
    # sha256_of: lambda(project-relative path) -> hex digest, or nil if gone
    def self.stale_files(analysis, sha256_of)
        (analysis['inputManifest'] || []).filter_map do |entry|
            entry['path'] if sha256_of.call(entry['path']) != entry['sha256']
        end
    end

    def self.count_words(n, (_, plural, singular))
        n == 1 ? "1 #{singular}" : "#{n} #{plural}"
    end

    # What the check read, for »Was der Workspace gelesen hat«.
    def self.describe(analysis, project)
        s = totals(analysis)
        p = analysis['project'] || {}
        ends = endings(analysis).size
        "Geschichte »#{p['title'] || 'ohne Titel'}« in `#{project}/#{p['pagesPath']}`: " \
            "#{s['pages'].to_i} Seiten, #{s['pageChoices'].to_i} Entscheidungen zu anderen Seiten, " \
            "#{s['localChoices'].to_i} lokale, #{s['conditions'].to_i} Bedingungen, #{s['scripts'].to_i} Skripte, " \
            "#{s['images'].to_i} Bilder, #{ends == 1 ? '1 Ende' : "#{ends} Enden"}; " \
            "#{s['errors'].to_i} Fehler, #{s['warnings'].to_i} Warnungen."
    end

    # BIF reports in English; the usual mistakes in the students' words,
    # everything else as BIF says it.
    def self.problem_text(item)
        source = item['source'] ? " `#{item['source'].to_s.gsub('`', "'")}`" : ''
        case item['code']
        when 'unreachable-page' then 'Diese Seite erreicht man von Seite 1 aus nicht. Führt eine Entscheidung zu ihr?'
        when 'missing-page' then "Diese Entscheidung führt zu Seite `#{item['target']}`, aber es gibt keine Datei `#{item['target']}.md`."
        when 'script-syntax' then "Im Skript stimmt etwas nicht (BIF: #{item['message']})."
        when 'condition-syntax' then "Die Bedingung#{source} ist kein gültiger JavaScript-Ausdruck."
        when 'expression-syntax' then "Der Ausdruck#{source} in `[[ … ]]` ist kein gültiger JavaScript-Ausdruck."
        when 'missing-image', 'missing-asset' then "Diese Datei gibt es nicht: #{item['message'].to_s.sub(/\AStory asset not found: /, '')}"
        when 'asset-outside-story' then 'Bilder und Medien gehören in den Ordner deiner Geschichte.'
        when 'pages-path-missing' then 'Den Ordner, der in `config.js` steht, gibt es nicht. Steht sein Name dort genau wie im Explorer, in Anführungszeichen?'
        when 'missing-start-page' then 'In deinem Geschichtenordner fehlt die `1.md`. Mit ihr beginnt jede Geschichte.'
        else item['message'].to_s
        end
    end

    def self.problem_line(item)
        "- `#{item['file']}`, Zeile #{item['line']}: #{problem_text(item)}"
    end

    # [ok, problem, message] for one story analysis.
    # analysis: the parsed analysis.json; project: folder (e.g. bif)
    def self.evaluate(analysis, expect, project:, sha256_of:)
        stale = stale_files(analysis, sha256_of)
        unless stale.empty?
            return [false, :stale, "BIF hat deine Geschichte zuletzt gelesen, bevor du `#{stale.first}` geändert hast. Öffne deinen Workspace und speichere eine Seite, dann liest BIF sie neu; danach kann ich nachsehen."]
        end

        summary = totals(analysis)
        pages_path = analysis.dig('project', 'pagesPath').to_s
        if expect['own_story'] && pages_path == STARTER
            return [false, :starter, "Deine Geschichte steht noch in `#{project}/#{STARTER}`. Lege einen eigenen Ordner an, etwa `pages-meine-geschichte`, und trag ihn in `#{project}/config.js` ein: `export const path = \"pages-meine-geschichte\";`"]
        end

        diagnostics = analysis['diagnostics'] || []
        if expect['no_errors']
            errors = diagnostics.select { |d| d['severity'] == 'error' }
            unless errors.empty?
                lines = errors.first(5).map { |d| problem_line(d) }.join("\n")
                return [false, :errors, "BIF meldet in deiner Geschichte #{errors.size == 1 ? 'einen Fehler' : "#{errors.size} Fehler"}, wie unten unter »Problems«:\n\n#{lines}"]
            end
        end

        MINIMUMS.each do |key, words|
            next unless expect.key?(key)
            have = summary[words[0]].to_i
            want = expect[key].to_i
            next if have >= want
            return [false, :"too_few_#{key}", "In deiner Geschichte stehen #{count_words(have, words)}, verlangt #{want == 1 ? 'ist' : 'sind'} #{count_words(want, words)}."]
        end

        if expect['all_reachable']
            unreachable = diagnostics.select { |d| d['code'] == 'unreachable-page' }
            unless unreachable.empty?
                lines = unreachable.first(5).map { |d| problem_line(d) }.join("\n")
                return [false, :unreachable, "Nicht jede Seite ist erreichbar:\n\n#{lines}"]
            end
        end

        if expect.key?('endings')
            have = endings(analysis).size
            want = expect['endings'].to_i
            if have < want
                return [false, :too_few_endings, have.zero? ?
                    'Deine Geschichte hat noch kein Ende: eine Seite, auf der keine Entscheidung mehr zu einer anderen Seite führt.' :
                    "Deine Geschichte hat #{have == 1 ? 'ein Ende' : "#{have} Enden"}, verlangt #{want == 1 ? 'ist eins' : "sind #{want}"}."]
            end
        end

        [true, nil, nil]
    end
end
