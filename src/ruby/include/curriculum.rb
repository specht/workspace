# The line of the curriculum each activity carries, which only logged-in
# teachers see: at the top of its page, in the Kata strip and on /lehrplan
# (CurriculumOverview). Seen through the plan of the state a teacher chose
# in the profile (docs/plan-bundeslaender.md): the plans are files in
# src/content/curricula/. Berlin and Brandenburg share one, the default; its
# line for an activity is the meta curriculum: (docs/lehrplan.md):
#
#   Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen); Oberstufe: Algorithmen und Datenstrukturen
#
# A plan without such lines derives one from the activity's topics (meta
# topics:, themen.yaml) through its own table, unless it has a line of its
# own for the activity. A part per level of the plan, in the plan's order,
# divided by »; «. In a part names divided by », «, each with details in
# brackets if wanted; only the details are free.
require 'cgi'
require 'yaml'

module Curriculum
    DIR = File.directory?('/src/content/curricula') ? '/src/content/curricula' : File.expand_path('../../content/curricula', __dir__)
    TOPICS_FILE = 'themen.yaml'
    DEFAULT_STATE = 'berlin'

    # The ages, a colour each on /lehrplan, youngest first: what the plans of
    # Berlin's Sek I name only, what both name, what only the Oberstufe's
    # names; age: (one of these) says whom an activity is for where its line
    # names both plans but it is meant for the younger. Every state has
    # these ages, whatever its plans are called; a plan's level says which
    # it takes (ages:).
    AGES = ['Sek I', 'Sek I und Oberstufe', 'Oberstufe'].freeze

    @cache = nil

    # Every plan and the topics, read again when a file changes.
    def self.data
        stamp = Dir.glob(File.join(DIR, '*.yaml')).sort.map { |f| [f, File.mtime(f).to_f] }
        return @cache[:data] if @cache && @cache[:stamp] == stamp

        topics = {}
        (YAML.load_file(File.join(DIR, TOPICS_FILE)) || {}).each do |area, list|
            (list || {}).each { |id, label| topics[id.to_s] = { :label => label.to_s, :area => area.to_s } }
        end
        plans = Dir.glob(File.join(DIR, '*.yaml')).sort.reject { |f| File.basename(f) == TOPICS_FILE }.map do |file|
            plan_from(File.basename(file, '.yaml'), YAML.load_file(file) || {})
        end
        # the default plan first, the others by title
        plans.sort_by! { |p| [p[:states].key?(DEFAULT_STATE) ? 0 : 1, p[:title]] }
        @cache = { :stamp => stamp, :data => { :topics => topics, :plans => plans } }
        @cache[:data]
    end

    def self.plan_from(key, y)
        levels = (y['levels'] || []).map do |l|
            {
                :level => l['level'].to_s, :title => l['title'].to_s, :id => l['id'].to_s,
                :source => l['source'].to_s, :url => l['url'], :source_for => l['source_for'] || {},
                # the plan itself as a PDF, for a state that has its own (pdf_for)
                :pdf => l['pdf'], :pdf_for => l['pdf_for'] || {}, :url_for => l['url_for'] || {},
                :ages => Array(l['ages'] || [0, 1, 2]).map(&:to_i),
                :groups => (l['groups'] || {}).map { |group, names| [group.to_s, Array(names).map(&:to_s)] },
            }
        end
        notes = {}
        (y['notes'] || {}).each { |level, names| (names || {}).each { |name, text| notes[[level.to_s, name.to_s]] = text.to_s } }
        topics = {}
        (y['topics'] || {}).each { |id, places| topics[id.to_s] = Array(places).map(&:to_s) }
        {
            :key => key, :title => y['title'].to_s, :states => (y['states'] || {}).transform_keys(&:to_s),
            :status => y['status'].to_s, :status_text => y['status_text'], :intro => y['intro'].to_s,
            :label => (y['label'] || 'Lehrplan').to_s, :levels => levels, :notes => notes, :topics => topics,
            :meta_lines => y['lines'] == 'meta', :lines => y['lines'].is_a?(Hash) ? y['lines'].transform_keys(&:to_s) : {},
        }
    end

    def self.plans
        data[:plans]
    end

    def self.topics
        data[:topics]
    end

    # [[state key, name, plan]] in the plans' order, for the choice in the profile.
    def self.states
        plans.flat_map { |plan| plan[:states].map { |key, name| [key, name, plan] } }
    end

    def self.state_name(state)
        (states.find { |key, _, _| key == state } || [])[1]
    end

    # The plan of a state; an unknown state or none gets the default.
    def self.plan_for(state)
        (states.find { |key, _, _| key == state.to_s } || states.find { |key, _, _| key == DEFAULT_STATE })[2]
    end

    def self.default_plan
        plan_for(DEFAULT_STATE)
    end

    def self.draft?(plan)
        plan[:status].to_s.downcase == 'entwurf'
    end

    # level => [names], in the plan's order
    def self.names(plan = nil)
        (plan || default_plan)[:levels].to_h { |l| [l[:level], l[:groups].flat_map(&:last)] }
    end

    # The plans of Berlin and Brandenburg, for code that knows only them.
    def self.sek_i
        names(default_plan)['Sek I']
    end

    def self.oberstufe
        names(default_plan)['Oberstufe']
    end

    # The age of an activity (an index of AGES): its age: if it has one,
    # otherwise by which levels of Berlin's plans its line names; one
    # without a line counts as both.
    def self.age_of(levels, age = nil)
        return AGES.index(age) if AGES.include?(age)

        sek = levels.include?('Sek I')
        ober = levels.include?('Oberstufe')
        return 0 if sek && !ober
        return 2 if ober && !sek

        1
    end

    def self.age_of_entry(entry)
        entry ||= {}
        age_of(entries(entry[:curriculum]).map(&:first).uniq, entry[:age])
    end

    # The line of an activity in a plan: its meta curriculum: for the plan of
    # Berlin and Brandenburg, a line of the plan's own, or one derived from
    # its topics (nil if nothing fits).
    def self.line(plan, slug, entry)
        plan ||= default_plan
        entry ||= {}
        return entry[:curriculum] if plan[:meta_lines]
        return plan[:lines][slug] if plan[:lines].key?(slug)

        derived_line(plan, entry)
    end

    # The topics' places in the plan, for the levels the activity's age
    # allows, merged into one line in the plan's order: a name once, its
    # details from every topic that leads to it.
    def self.derived_line(plan, entry)
        age = age_of_entry(entry)
        allowed = plan[:levels].select { |l| l[:ages].include?(age) }.map { |l| l[:level] }
        found = {}
        Array(entry[:topics]).each do |topic|
            (plan[:topics][topic.to_s] || []).each do |place|
                level, name, details = entries(place, plan).first
                next unless level && allowed.include?(level)

                found[[level, name]] ||= []
                details.to_s.split(', ').each { |d| found[[level, name]] << d unless d.empty? || found[[level, name]].include?(d) } if details
            end
        end
        return nil if found.empty?

        order = names(plan)
        parts = plan[:levels].map { |l| l[:level] }.filter_map do |level|
            list = order[level].select { |name| found.key?([level, name]) }
            next if list.empty?

            "#{level}: " + list.map { |name| details = found[[level, name]]; details.empty? ? name : "#{name} (#{details.join(', ')})" }.join(', ')
        end
        parts.join('; ')
    end

    # The anchor of a name on /lehrplan: »sek-i-information-und-daten«.
    def self.anchor(level, name)
        "#{level} #{name}".downcase.tr('äöüß', 'aous').gsub(/[^a-z0-9]+/, '-').gsub(/\A-|-\z/, '')
    end

    # A line as HTML: each name a link to its place on /lehrplan, the details
    # as text; a line that is not right as plain text.
    def self.linked_html(line, plan = nil)
        h = ->(s) { CGI.escapeHTML(s.to_s) }
        entries = entries(line, plan)
        return h.(line.to_s.strip) if entries.empty?

        entries.chunk_while { |a, b| a[0] == b[0] }.map do |part|
            names = part.map do |level, name, details|
                "<a href='/lehrplan##{anchor(level, name)}'>#{h.(name)}</a>#{details ? " (#{h.(details)})" : ''}"
            end
            "#{h.(part[0][0])}: #{names.join(', ')}"
        end.join('; ')
    end

    # A right line as its entries, in its order: [level, name, details]
    # (details nil without brackets). [] for a line that is not right.
    def self.entries(line, plan = nil)
        return [] unless problems(line, plan).empty?

        levels = level_pattern(plan)
        split(line.to_s.strip, ';').flat_map do |part|
            level = part[levels, 1]
            split(part.delete_prefix("#{level}: "), ',').map do |item|
                name = item.sub(/\s*\(.*\)\z/m, '')
                details = item[/\((.*)\)\z/m, 1]
                [level, name, details]
            end
        end
    end

    def self.level_pattern(plan)
        /\A(#{names(plan).keys.map { |l| Regexp.escape(l) }.join('|')}): /
    end

    # What is wrong with a line, as messages; [] when it is right.
    def self.problems(line, plan = nil)
        text = line.to_s.strip
        return ['no line'] if text.empty?

        all = names(plan)
        pattern = level_pattern(plan)
        parts = split(text, ';')
        levels = parts.map { |part| part[pattern, 1] }
        return ["each part begins with one of #{all.keys.map { |l| "»#{l}: «" }.join(', ')}: #{text}"] if levels.include?(nil)
        return ["#{all.keys.join(' before ')}, each once: #{text}"] unless levels == levels.uniq && levels == all.keys & levels

        parts.zip(levels).flat_map do |part, level|
            split(part.delete_prefix("#{level}: "), ',').map { |item| item.sub(/\s*\(.*\)\z/m, '') }.filter_map do |name|
                "not a name of the plan for #{level}: »#{name}«" unless all[level].include?(name)
            end
        end
    end

    # text split at »<mark> « outside brackets, each piece stripped: the
    # parts of a line at »; «, the names of a part at », «. A bracket left
    # open keeps the rest in one piece, so that it is no name.
    def self.split(text, mark)
        pieces = ['']
        depth = 0
        text.each_char.with_index do |c, i|
            depth += 1 if c == '('
            depth -= 1 if c == ')'
            if depth.zero? && c == mark && text[i + 1] == ' '
                pieces << ''
            else
                pieces[-1] += c
            end
        end
        pieces.map(&:strip)
    end
end
