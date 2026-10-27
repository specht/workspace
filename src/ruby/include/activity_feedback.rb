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
    # »Looks finished«: nobody of the group's students has checked,
    # answered or handed in a task of the activity for QUIET_DAYS, and at
    # least MIN_STUDENTS of them (all, in a smaller group) did so before.
    # Older than ASK_WITHIN_DAYS, the card does not come by itself any
    # more (nobody remembers a lesson of last term); the link stays.
    QUIET_DAYS = 7
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
    # group has. :answered, :never, :not_worked, :in_progress, :too_old,
    # :snoozed or :due (the card shows).
    def self.prompt(feedback:, last:, students:, size:, now:)
        feedback ||= {}
        return :answered if feedback[:created_at]
        return :never if feedback[:never] == true
        return :not_worked if last.nil? || students.to_i < [[MIN_STUDENTS, size.to_i].min, 1].max

        quiet = now.to_i - last.to_i
        return :in_progress if quiet < QUIET_DAYS * DAY
        return :too_old if quiet > ASK_WITHIN_DAYS * DAY
        return :snoozed if feedback[:snoozed_until].to_i > now.to_i

        :due
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

    # rows: [{ activity:, title:, slug:, group:, teacher:, email:, feedback: node }]
    # -> [{ activity:, title:, slug:, count:, newest:, counts: { 'overall' => { 'good' => n } },
    #       languages: { id => n }, languages_texts: [..], entries: [...] }], newest first.
    def self.summary(rows)
        rows.group_by { |r| r[:activity] }.map do |activity, list|
            entries = list.map do |r|
                f = r[:feedback].transform_keys(&:to_s)
                {
                    :group => f['group'] || r[:group], :former_group => f['group'].nil? ? f['former_group'] : nil,
                    :teacher => r[:teacher], :email => f['contact'] == true ? r[:email] : nil,
                    :at => (f['updated_at'] || f['created_at']).to_i, :answers => f,
                }
            end.sort_by { |e| -e[:at] }
            counts = CHOICES.to_h do |key, choices|
                [key, choices.to_h { |value, _| [value, entries.count { |e| e[:answers][key] == value }] }]
            end
            languages = CodeLanguages::LANGUAGES.to_h { |id| [id, entries.count { |e| Array(e[:answers]['languages']).include?(id) }] }
            {
                :activity => activity, :title => list.first[:title], :slug => list.first[:slug], :count => entries.size,
                :newest => entries.first[:at], :counts => counts, :languages => languages.select { |_, n| n > 0 },
                :languages_texts => entries.filter_map { |e| e[:answers]['languages_text'] },
                :entries => entries,
            }
        end.sort_by { |a| -a[:newest] }
    end

    def self.admin_html(summary)
        io = +''
        if summary.empty?
            return "<p class='feedback-none'>Noch keine Rückmeldung. Lehrkräfte geben sie auf der Klassenseite, im Reiter »Fortschritt« einer Aktivität.</p>"
        end

        total = summary.sum { |a| a[:count] }
        io << "<p class='feedback-total'>#{total} #{total == 1 ? 'Rückmeldung' : 'Rückmeldungen'} zu #{summary.size} " \
            "#{summary.size == 1 ? 'Aktivität' : 'Aktivitäten'}, die neueste zuerst.</p>"
        summary.each do |a|
            title = a[:slug] ? "<a href='/#{h(a[:slug])}'>#{h(a[:title])}</a>" : h(a[:title] || a[:activity])
            io << "<section class='feedback-activity'><h2 class='feedback-activity-title'>#{title} " \
                "<span class='feedback-activity-count'>#{a[:count]} #{a[:count] == 1 ? 'Rückmeldung' : 'Rückmeldungen'}</span></h2>"
            io << "<div class='feedback-table-scroll'><table class='table table-sm feedback-counts'><tbody>"
            CHOICES.each do |key, choices|
                io << "<tr><th scope='row'>#{QUESTIONS[key]}</th>"
                choices.each do |value, text|
                    n = a[:counts][key][value]
                    io << "<td class='#{n.zero? ? 'feedback-zero' : ''}'>#{h(text)} <strong>#{n}</strong></td>"
                end
                io << '</tr>'
            end
            wished = a[:languages].map { |id, n| "#{h(CodeLanguages::NAMES[id])} <strong>#{n}</strong>" }
            wished += a[:languages_texts].map { |t| "»#{h(t)}«" }
            io << "<tr><th scope='row'>Sprachen</th><td colspan='3'>#{wished.empty? ? '–' : wished.join(', ')}</td></tr>"
            io << '</tbody></table></div>'
            a[:entries].each do |e|
                f = e[:answers]
                group = e[:former_group] ? "#{h(e[:former_group])} (Gruppe gelöscht)" : h(e[:group])
                io << "<article class='feedback-entry'><p class='feedback-entry-head'><strong>#{group}</strong>, #{h(e[:teacher])}, #{date(e[:at])}"
                io << ", <a href='mailto:#{h(e[:email])}'>#{h(e[:email])}</a> <span class='feedback-contact-ok'>(darf angeschrieben werden)</span>" if e[:email]
                io << '</p>'
                parts = CHOICES.keys.filter_map do |key|
                    text = label(key, f[key])
                    next unless text

                    extra = key == 'duration' && !f['duration_planned'].to_s.empty? ? " (angesetzt: #{h(f['duration_planned'])})" : ''
                    "#{QUESTIONS[key]}: #{h(text)}#{extra}"
                end
                languages = Array(f['languages']).map { |id| CodeLanguages::NAMES[id.to_s] }.compact
                languages << f['languages_text'] if f['languages_text']
                parts << "Sprachen: #{languages.map { |l| h(l) }.join(', ')}" unless languages.empty?
                io << "<ul class='feedback-entry-answers'>#{parts.map { |p| "<li>#{p}</li>" }.join}</ul>" unless parts.empty?
                io << "<p class='feedback-entry-text'><span class='feedback-entry-label'>Ein Satz dazu:</span> #{h(f['overall_text'])}</p>" if f['overall_text']
                if f['text']
                    io << "<p class='feedback-entry-text'><span class='feedback-entry-label'>Gefehlt, gestört, gut funktioniert:</span><br>" \
                        "#{h(f['text']).gsub("\n", '<br>')}</p>"
                end
                io << '</article>'
            end
            io << '</section>'
        end
        io
    end
end
