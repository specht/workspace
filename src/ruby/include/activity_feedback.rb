require 'cgi'
require 'json'
require 'time'
require_relative 'code_languages'

# Feedback of teachers on an activity they did with a group (docs/teacher-access.md,
# »Rückmeldung zu einer Aktivität«): how it went, the duration, the level,
# which programming language they would like it in, a text, and whether the
# administration may write to them about it. Asked on the class page once
# the group looks finished with the activity, never in a modal of its own;
# the administration reads it under /rueckmeldungen.
#
# In Neo4j, one node per teacher, group and activity:
#   (:ActivityFeedback {group, activity, slug, overall, overall_text,
#       duration, duration_planned, level, languages, languages_text, text,
#       contact, created_at, updated_at, snoozed_until, never})-[:BY]->(:User)
# created_at is set with the first answer; a node without it only holds
# »Nicht jetzt« (snoozed_until) or »Nicht mehr fragen« (never). It goes with
# the teacher's account (AccountDeletion); when the group is deleted, an
# answered one keeps the group's name as former_group, the rest goes
# (GroupData).
#
# Plain Ruby, tested on its own (test/activity_feedback_test.rb); Main
# fetches the data (include/activity_feedback_page.rb).
module ActivityFeedback
    DAY = 24 * 3600
    # »Looks finished«: at least MIN_STUDENTS of the group's students (all,
    # in a smaller group) have a result in the activity, and either the
    # class has moved on (in one other activity, at least half of those
    # students and at least as many as MIN_STUDENTS asks have a result newer
    # than the last one here: a few students trying something else are no
    # moving on) and nothing came here for QUIET_DAYS, or nothing came for
    # QUIET_ALONE_DAYS (a class that stops after an activity). A week of
    # holidays or a cancelled lesson alone does not bring the card in the
    # middle of an activity. Older than ASK_WITHIN_DAYS, the card does not
    # come by itself any more (nobody remembers a lesson of last term); the
    # button stays.
    QUIET_DAYS = 7
    QUIET_ALONE_DAYS = 21
    ASK_WITHIN_DAYS = 60
    MIN_STUDENTS = 3
    # »Nicht jetzt«
    SNOOZE_DAYS = 14

    OVERALL = [['good', 'gut'], ['mixed', 'teils, teils'], ['bad', 'nicht gut']].freeze
    DURATION = [['shorter', 'kürzer'], ['fit', 'passte'], ['longer', 'länger']].freeze
    LEVEL = [['easy', 'zu leicht'], ['fit', 'passend'], ['hard', 'zu schwer']].freeze
    CHOICES = { 'overall' => OVERALL, 'duration' => DURATION, 'level' => LEVEL }.freeze
    TEXTS = { 'overall_text' => 300, 'languages_text' => 200, 'text' => 3000 }.freeze
    QUESTIONS = { 'overall' => 'Insgesamt', 'duration' => 'Dauer', 'level' => 'Niveau' }.freeze

    # What the form sends, as it is stored: the choices among their
    # values, the texts stripped and cut, the languages among
    # CodeLanguages::LANGUAGES (without those the activity has), contact
    # true or false. nil when nothing is answered (contact alone is no
    # answer).
    def self.normalize(raw, offered: [])
        raw = raw.is_a?(Hash) ? raw.transform_keys(&:to_s) : {}
        answers = {}
        CHOICES.each do |key, choices|
            value = raw[key].to_s
            answers[key] = choices.any? { |v, _| v == value } ? value : nil
        end
        TEXTS.each do |key, max|
            text = raw[key].to_s.gsub("\r\n", "\n").strip[0, max]
            answers[key] = text.empty? ? nil : text
        end
        answers['languages'] = Array(raw['languages']).map(&:to_s) & (CodeLanguages::LANGUAGES - offered)
        answers['contact'] = raw['contact'] == true
        answered?(answers) ? answers : nil
    end

    def self.answered?(answers)
        return false unless answers

        a = answers.transform_keys(&:to_s)
        (CHOICES.keys + TEXTS.keys).any? { |key| !a[key].to_s.empty? } || !Array(a['languages']).empty?
    end

    # Whether the class page asks, for one teacher, group and activity:
    # feedback: the stored node (symbol keys) or nil; last: the newest
    # TaskState.updated of the group's students in the activity (epoch) or
    # nil; students: how many of them have one; size: how many students the
    # group has; moved_on: the most of the group's students that have a
    # result newer than last in one other activity (moved_on_count).
    # :answered, :never, :not_worked, :in_progress, :too_old, :snoozed or
    # :due (the card shows).
    def self.prompt(feedback:, last:, students:, size:, now:, moved_on: 0)
        feedback ||= {}
        return :answered if feedback[:created_at]
        return :never if feedback[:never] == true
        need = [[MIN_STUDENTS, size.to_i].min, 1].max
        return :not_worked if last.nil? || students.to_i < need

        quiet = now.to_i - last.to_i
        class_moved_on = moved_on.to_i >= [(students.to_i / 2.0).ceil, need].max
        return :in_progress if quiet < (class_moved_on ? QUIET_DAYS : QUIET_ALONE_DAYS) * DAY
        return :too_old if quiet > ASK_WITHIN_DAYS * DAY
        return :snoozed if feedback[:snoozed_until].to_i > now.to_i

        :due
    end

    # moved_on for prompt: newest is { email => { activity => newest result
    # (epoch) } } for the group's students; the most of them with a result
    # newer than last in one activity other than this one.
    def self.moved_on_count(newest, activity, last)
        counts = Hash.new(0)
        newest.each_value do |per_activity|
            per_activity.each { |other, t| counts[other] += 1 if other != activity && t.to_i > last.to_i }
        end
        counts.values.max || 0
    end

    # The newest result and how many students have one, from the board's
    # states ({ email => { task_id => TaskState } }), counting only students.
    def self.results(states, students)
        lasts = students.filter_map do |email|
            times = (states[email] || {}).values.map { |s| s[:updated].to_i }.reject(&:zero?)
            times.max
        end
        { :last => lasts.max, :students => lasts.size }
    end

    # The languages an activity has among the five: those it offers to
    # switch between (languages:), else those its line »language:« names
    # (»Ruby / Python«, »C« has none of them).
    def self.current_languages(content)
        offered = Array(content[:languages]).map(&:to_s) & CodeLanguages::LANGUAGES
        return offered unless offered.empty?

        names = content[:language].to_s.split(%r{\s*/\s*}).map { |n| n.strip.downcase }
        CodeLanguages::LANGUAGES.select { |id| names.include?(CodeLanguages::NAMES[id].downcase) }
    end

    def self.label(key, value)
        (CHOICES[key] || []).find { |v, _| v == value.to_s }&.last
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.date(epoch)
        epoch ? Time.at(epoch.to_i).strftime('%d.%m.%Y') : ''
    end

    def self.days_text(days)
        days == 1 ? 'einem Tag' : "#{days} Tagen"
    end

    # --- The class page ---------------------------------------------------

    # In the toolbar of »Fortschritt«: always there, for one who wants to
    # answer earlier, or to change the answer.
    def self.open_button_html(answered)
        label = answered ? 'Deine Rückmeldung' : 'Rückmeldung geben'
        "<button type='button' class='btn btn-sm btn-outline-secondary feedback-open' aria-controls='feedback-form'>" \
            "<i class='bi bi-chat-square-text' aria-hidden='true'></i>#{label}</button>"
    end

    # The card above the board, when prompt is :due: what it is about,
    # three buttons. days: since the last result.
    def self.card_html(group, title, days)
        "<aside class='feedback-card' aria-labelledby='feedback-card-title'>" \
            "<i class='bi bi-chat-square-text' aria-hidden='true'></i>" \
            "<div class='feedback-card-body'><h3 class='feedback-card-title' id='feedback-card-title'>Wie lief die Aktivität?</h3>" \
            "<p>Seit #{days_text(days)} kam aus #{h(group)} keine neue Antwort zu »#{h(title)}«. Wenn ihr damit fertig seid: " \
            'Wie lief es? Deine Rückmeldung dauert eine Minute und hilft, die Aktivität besser zu machen.</p>' \
            "<div class='feedback-card-actions'>" \
            "<button type='button' class='btn btn-sm btn-primary feedback-open' aria-controls='feedback-form'>Rückmeldung geben</button>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary feedback-dismiss' data-mode='later' " \
            "title='Fragt in #{SNOOZE_DAYS} Tagen wieder'>Nicht jetzt</button>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary feedback-dismiss' data-mode='never' " \
            "title='Fragt zu dieser Aktivität und Gruppe nicht mehr; geben kannst du sie trotzdem'>Nicht mehr fragen</button>" \
            '</div></div></aside>'
    end

    # The form, hidden until opened (the card, the button, #rueckmeldung).
    # info: { group:, activity:, title:, duration:, language:, current: [ids],
    # email: }; stored: the node (symbol keys) or nil.
    def self.form_html(info, stored)
        s = (stored || {}).transform_keys(&:to_s)
        answered = !s['created_at'].nil?
        radio = lambda do |key, legend, hint|
            io = +"<fieldset class='feedback-question'><legend>#{legend}</legend>"
            io << "<p class='feedback-hint-line'>#{hint}</p>" if hint
            io << "<div class='feedback-choices'>"
            CHOICES[key].each do |value, text|
                id = "feedback-#{key}-#{value}"
                checked = s[key] == value ? ' checked' : ''
                io << "<input type='radio' class='btn-check' name='#{key}' id='#{id}' value='#{value}' autocomplete='off'#{checked}>" \
                    "<label class='btn btn-sm btn-outline-secondary' for='#{id}'>#{h(text)}</label>"
            end
            io << '</div>'
        end
        io = +"<form class='feedback-form' id='feedback-form' data-group='#{h(info[:group])}' data-activity='#{h(info[:activity])}' " \
            "aria-labelledby='feedback-form-title' hidden>"
        io << "<h3 class='feedback-form-title' id='feedback-form-title'>Rückmeldung zu »#{h(info[:title])}« mit #{h(info[:group])}</h3>"
        io << "<p class='feedback-form-note'>Alles ist freiwillig, eine Antwort genügt. Lesen kann sie nur die Administration, " \
            'die die Aktivitäten schreibt; deine Klasse sieht nichts davon. Bitte schreib keine Namen von Schülerinnen und Schülern hinein.</p>'
        if answered
            io << "<p class='feedback-form-saved'><i class='bi bi-check-lg' aria-hidden='true'></i>Gespeichert am #{date(s['updated_at'] || s['created_at'])}. " \
                'Du kannst sie hier ändern.</p>'
        end
        io << radio.call('overall', 'Wie lief es insgesamt?', nil)
        io << "<input type='text' class='form-control form-control-sm feedback-text-short' name='overall_text' maxlength='#{TEXTS['overall_text']}' " \
            "aria-label='Ein Satz dazu' placeholder='Ein Satz dazu (freiwillig)' value='#{h(s['overall_text'])}'></fieldset>"
        planned = info[:duration].to_s.strip
        io << radio.call('duration', 'Die Dauer', planned.empty? ? nil : "Angesetzt: #{h(planned)}") << '</fieldset>'
        io << radio.call('level', 'Das Niveau für die Gruppe', nil) << '</fieldset>'
        wished = Array(s['languages']).map(&:to_s)
        current = Array(info[:current])
        io << "<fieldset class='feedback-question'><legend>In welcher Programmiersprache hättest du die Aktivität gern (zusätzlich)?</legend>"
        io << "<p class='feedback-hint-line'>Jetzt: #{h(info[:language])}</p>" unless info[:language].to_s.strip.empty?
        io << "<div class='feedback-choices'>"
        CodeLanguages::LANGUAGES.each do |id|
            name = CodeLanguages::NAMES[id]
            box = "feedback-language-#{id}"
            if current.include?(id)
                io << "<input type='checkbox' class='btn-check' id='#{box}' disabled autocomplete='off'>" \
                    "<label class='btn btn-sm btn-outline-secondary feedback-language-current' for='#{box}' title='Gibt es schon'>#{h(name)} <span class='feedback-language-note'>gibt es schon</span></label>"
            else
                io << "<input type='checkbox' class='btn-check' name='languages' id='#{box}' value='#{id}' autocomplete='off'#{wished.include?(id) ? ' checked' : ''}>" \
                    "<label class='btn btn-sm btn-outline-secondary' for='#{box}'>#{h(name)}</label>"
            end
        end
        io << '</div>'
        io << "<input type='text' class='form-control form-control-sm feedback-text-short' name='languages_text' maxlength='#{TEXTS['languages_text']}' " \
            "aria-label='Eine andere Sprache' placeholder='Eine andere Sprache' value='#{h(s['languages_text'])}'></fieldset>"
        io << "<div class='feedback-question'><label class='form-label' for='feedback-text'>Was hat gefehlt, gestört oder besonders gut funktioniert?</label>" \
            "<textarea class='form-control' id='feedback-text' name='text' rows='4' maxlength='#{TEXTS['text']}'>#{h(s['text'])}</textarea></div>"
        io << "<div class='form-check feedback-contact'><input class='form-check-input' type='checkbox' name='contact' id='feedback-contact'#{s['contact'] == true ? ' checked' : ''}>" \
            "<label class='form-check-label' for='feedback-contact'>Die Administration darf mir dazu an #{h(info[:email])} schreiben.</label></div>"
        io << "<p class='feedback-form-error' role='alert' hidden>Gib mindestens eine Antwort, dann lässt sich die Rückmeldung senden.</p>"
        io << "<div class='feedback-form-actions'><button type='submit' class='btn btn-primary btn-sm'><i class='bi bi-send' aria-hidden='true'></i>" \
            "#{answered ? 'Änderung speichern' : 'Rückmeldung senden'}</button>" \
            "<button type='button' class='btn btn-outline-secondary btn-sm feedback-cancel'>Abbrechen</button></div>"
        io << '</form>'
    end

    # On the teacher's start page: one card that waits, at most (the newest).
    def self.home_hint_html(group, activity, title, days, link)
        "<aside class='feedback-home' aria-label='Rückmeldung zu einer Aktivität' data-group='#{h(group)}' data-activity='#{h(activity)}' hidden>" \
            "<i class='bi bi-chat-square-text' aria-hidden='true'></i>" \
            "<div class='feedback-home-body'><strong>Wie lief »#{h(title)}« mit #{h(group)}?</strong> " \
            "Seit #{days_text(days)} kam keine neue Antwort dazu. Eine Rückmeldung dauert eine Minute.</div>" \
            "<div class='feedback-home-actions'><a class='btn btn-sm btn-primary' href='#{h(link)}'>Rückmeldung geben</a>" \
            "<button type='button' class='btn btn-sm btn-outline-secondary feedback-home-close'>Nicht jetzt</button></div></aside>"
    end

    # --- The administration's page ------------------------------------------
    #
    # The administration opens it now and then to see which activities need
    # work: first an overview, one card per activity (how it went as a bar,
    # duration and level in a few words, the languages wished for), the
    # newest feedback first; below, per activity, every answer with its
    # texts. Filters (activity_feedback.js): an activity's name, »Nur mit
    # Text«, »Kontakt erwünscht«.

    # How »how it went« is drawn: colour class and icon per answer.
    OVERALL_LOOK = {
        'good' => ['fb-good', 'bi-emoji-smile'],
        'mixed' => ['fb-mixed', 'bi-emoji-neutral'],
        'bad' => ['fb-bad', 'bi-emoji-frown'],
    }.freeze

    # rows: [{ activity:, title:, slug:, duration:, group:, teacher:, email:, feedback: node }]
    # -> [{ activity:, title:, slug:, duration:, count:, newest:, counts: { 'overall' => { 'good' => n } },
    #       languages: { id => n } (most wished first), languages_texts: [..], entries: [...] }], newest first.
    def self.summary(rows)
        rows.group_by { |r| r[:activity] }.map do |activity, list|
            entries = list.map do |r|
                f = r[:feedback].transform_keys(&:to_s)
                {
                    :group => f['group'] || r[:group], :former_group => f['group'].nil? ? f['former_group'] : nil,
                    :teacher => r[:teacher], :email => f['contact'] == true ? r[:email] : nil,
                    :at => (f['updated_at'] || f['created_at']).to_i, :answers => f,
                    :has_text => %w[overall_text text].any? { |key| !f[key].to_s.empty? },
                }
            end.sort_by { |e| -e[:at] }
            counts = CHOICES.to_h do |key, choices|
                [key, choices.to_h { |value, _| [value, entries.count { |e| e[:answers][key] == value }] }]
            end
            languages = CodeLanguages::LANGUAGES.to_h { |id| [id, entries.count { |e| Array(e[:answers]['languages']).include?(id) }] }
            {
                :activity => activity, :title => list.first[:title], :slug => list.first[:slug], :duration => list.first[:duration],
                :count => entries.size, :newest => entries.first[:at], :counts => counts,
                :languages => languages.select { |_, n| n > 0 }.sort_by { |id, n| [-n, CodeLanguages::LANGUAGES.index(id)] }.to_h,
                :languages_texts => entries.filter_map { |e| e[:answers]['languages_text'] },
                :entries => entries,
            }
        end.sort_by { |a| -a[:newest] }
    end

    # »2× länger, 1× passte«: the answers given, the most frequent first;
    # nil when nobody answered the question.
    def self.tally_text(key, counts)
        given = CHOICES[key].filter_map { |value, text| [counts[value], text] if counts[value].to_i > 0 }
        return nil if given.empty?

        given.sort_by { |n, _| -n }.map { |n, text| "#{n}× #{text}" }.join(', ')
    end

    def self.anchor(activity)
        "fb-#{activity.to_s.gsub(/[^a-z0-9_-]+/i, '-')}"
    end

    def self.plural(n, one, many)
        "#{n} #{n == 1 ? one : many}"
    end

    def self.title_html(a)
        a[:slug] ? "<a href='/#{h(a[:slug])}'>#{h(a[:title])}</a>" : h(a[:title] || a[:activity])
    end

    # The bar of »how it went«: one segment per answer, as wide as its
    # count; grey when nobody answered the question.
    def self.bar_html(counts)
        total = counts.values.sum
        label = OVERALL.map { |value, text| "#{text} #{counts[value]}" }.join(', ')
        return "<div class='fb-bar fb-bar-empty' role='img' aria-label='Insgesamt: keine Angabe'></div>" if total.zero?

        segments = OVERALL.filter_map do |value, _|
            next if counts[value].zero?

            "<span class='#{OVERALL_LOOK[value][0]}' style='flex-grow: #{counts[value]}'></span>"
        end
        "<div class='fb-bar' role='img' aria-label='Insgesamt: #{h(label)}'>#{segments.join}</div>"
    end

    def self.overall_counts_html(counts)
        OVERALL.map do |value, text|
            cls, icon = OVERALL_LOOK[value]
            "<span class='fb-count #{cls}#{counts[value].zero? ? ' fb-zero' : ''}'><i class='bi #{icon}' aria-hidden='true'></i>#{h(text)} <strong>#{counts[value]}</strong></span>"
        end.join
    end

    def self.languages_html(a)
        badges = a[:languages].map { |id, n| "<span class='badge fb-lang'>#{h(CodeLanguages::NAMES[id])} #{n}</span>" }
        badges += a[:languages_texts].uniq.map { |t| "<span class='badge fb-lang fb-lang-other'>#{h(t)}</span>" }
        badges.join(' ')
    end

    def self.overview_card_html(a)
        texts = a[:entries].count { |e| e[:has_text] }
        contact = a[:entries].any? { |e| e[:email] }
        io = +"<article class='fb-card' data-title='#{h(a[:title].to_s.downcase)}' data-text='#{texts > 0 ? 1 : 0}' data-contact='#{contact ? 1 : 0}'>"
        io << "<div class='fb-card-head'><h3 class='fb-card-title'>#{title_html(a)}</h3>" \
            "<span class='fb-muted'>#{plural(a[:count], 'Rückmeldung', 'Rückmeldungen')}, zuletzt #{date(a[:newest])}</span></div>"
        io << bar_html(a[:counts]['overall'])
        io << "<div class='fb-counts'>#{overall_counts_html(a[:counts]['overall'])}</div>"
        io << "<dl class='fb-facts'>"
        duration = tally_text('duration', a[:counts]['duration'])
        planned = a[:duration].to_s.strip.empty? ? '' : "<span class='fb-muted fb-planned'>angesetzt: #{h(a[:duration])}</span>"
        io << "<div><dt><i class='bi bi-clock' aria-hidden='true'></i>Dauer</dt><dd>#{duration ? h(duration) : '–'}#{planned}</dd></div>"
        level = tally_text('level', a[:counts]['level'])
        io << "<div><dt><i class='bi bi-speedometer2' aria-hidden='true'></i>Niveau</dt><dd>#{level ? h(level) : '–'}</dd></div>"
        languages = languages_html(a)
        io << "<div><dt><i class='bi bi-code-slash' aria-hidden='true'></i>Gewünscht</dt><dd>#{languages.empty? ? '–' : languages}</dd></div>"
        io << '</dl>'
        link = texts > 0 ? "#{plural(texts, 'Text', 'Texte')} lesen" : 'Alle Antworten'
        io << "<a class='fb-to-texts' href='##{anchor(a[:activity])}'><i class='bi bi-chat-left-text' aria-hidden='true'></i>#{link}</a>"
        io << '</article>'
    end

    def self.entry_html(e)
        f = e[:answers]
        group = e[:former_group] ? "#{h(e[:former_group])} (Gruppe gelöscht)" : h(e[:group])
        io = +"<article class='fb-entry' data-text='#{e[:has_text] ? 1 : 0}' data-contact='#{e[:email] ? 1 : 0}'>"
        chips = CHOICES.keys.filter_map do |key|
            text = label(key, f[key])
            next unless text

            cls = key == 'overall' ? " #{OVERALL_LOOK[f[key]][0]}" : ''
            prefix = { 'overall' => '', 'duration' => 'Dauer: ', 'level' => 'Niveau: ' }[key]
            "<span class='fb-chip#{cls}'>#{prefix}#{h(text)}</span>"
        end
        languages = Array(f['languages']).map { |id| CodeLanguages::NAMES[id.to_s] }.compact
        languages << f['languages_text'] if f['languages_text']
        chips << "<span class='fb-chip'>gewünscht: #{languages.map { |l| h(l) }.join(', ')}</span>" unless languages.empty?
        io << "<div class='fb-chips'>#{chips.join}</div>" unless chips.empty?
        io << "<p class='fb-text'>#{h(f['overall_text'])}</p>" if f['overall_text']
        io << "<p class='fb-text'>#{h(f['text']).gsub("\n", '<br>')}</p>" if f['text']
        io << "<p class='fb-entry-meta'>#{group}, #{h(e[:teacher])}, #{date(e[:at])}"
        if e[:email]
            io << "<a class='fb-mail' href='mailto:#{h(e[:email])}' title='Hat angekreuzt, dass die Administration schreiben darf'>" \
                "<i class='bi bi-envelope' aria-hidden='true'></i>#{h(e[:email])}</a>"
        end
        io << '</p></article>'
    end

    EMPTY = "<div class='fb-empty'><i class='bi bi-chat-square-text' aria-hidden='true'></i><div><p><strong>Noch keine Rückmeldung.</strong></p>" \
        '<p>Lehrkräfte geben sie auf der Klassenseite, im Reiter »Fortschritt« einer Aktivität (»Rückmeldung geben«). Von selbst fragt die Seite danach, ' \
        'wenn mindestens drei aus einer Gruppe mit einer Aktivität gearbeitet haben und eine Woche lang nichts mehr dazu kam.</p>' \
        '<p>Hier steht dann je Aktivität, wie es lief, ob Dauer und Niveau passten und welche Sprachen gewünscht sind, darunter jeder Text.</p></div></div>'.freeze

    def self.admin_html(summary)
        return EMPTY if summary.empty?

        total = summary.sum { |a| a[:count] }
        io = +"<form class='fb-filters' role='search' onsubmit='return false'>" \
            "<input type='search' class='form-control form-control-sm fb-search' placeholder='Aktivität suchen' aria-label='Aktivität suchen'>" \
            "<input type='checkbox' class='btn-check' id='fb-only-text' autocomplete='off'>" \
            "<label class='btn btn-sm btn-outline-secondary' for='fb-only-text'><i class='bi bi-chat-left-text' aria-hidden='true'></i>Nur mit Text</label>" \
            "<input type='checkbox' class='btn-check' id='fb-only-contact' autocomplete='off'>" \
            "<label class='btn btn-sm btn-outline-secondary' for='fb-only-contact'><i class='bi bi-envelope' aria-hidden='true'></i>Kontakt erwünscht</label>" \
            "<span class='fb-muted fb-total'>#{plural(total, 'Rückmeldung', 'Rückmeldungen')} zu #{plural(summary.size, 'Aktivität', 'Aktivitäten')}</span></form>"
        io << "<p class='fb-none-found' hidden>Keine Rückmeldung passt zu diesem Filter.</p>"
        io << "<h2 class='fb-heading'>Überblick</h2><div class='fb-overview'>#{summary.map { |a| overview_card_html(a) }.join}</div>"
        io << "<h2 class='fb-heading'>Antworten und Texte</h2>"
        summary.each do |a|
            io << "<section class='fb-activity' id='#{anchor(a[:activity])}' data-title='#{h(a[:title].to_s.downcase)}'>" \
                "<h3 class='fb-activity-title'>#{title_html(a)} <span class='fb-muted'>#{plural(a[:count], 'Rückmeldung', 'Rückmeldungen')}</span></h3>"
            a[:entries].each { |e| io << entry_html(e) }
            io << '</section>'
        end
        io
    end
end
