require 'cgi'
require 'date'

# What is used, and where tasks are hard (/statistik, administrators only;
# docs/datenschutz.md, »Nutzung ohne Namen«): from the attempts the checks
# leave (TaskAttempt: task, status, time, no answer), counted per school,
# activity and task, without names. Every number that rests on fewer than
# MIN_PEOPLE different students is hidden (»< 5«), and with it what would
# give it away (the checks of that cell, its tasks). Plain Ruby, tested on
# its own (test/usage_stats_test.rb); the page (usage_stats_page.rb) hands
# in the rows with a number per person instead of an address.
module UsageStats
    MIN_PEOPLE = 5
    WINDOWS = [7, 30, 90].freeze
    DEFAULT_WINDOW = 30
    STUMBLING = 15

    # rows: [{ person:, school:, activity:, task:, status:, ts: }], person a
    # number that stands for one student within this request.
    def self.summary(rows, days:, now: Time.now.to_i, min: MIN_PEOPLE)
        since = now - days * 86_400
        rows = rows.select { |r| r[:ts].to_i >= since }
        count = ->(people) { people.size >= min ? people.size : nil }
        {
            :days => days,
            :min => min,
            :people => count.(people(rows)),
            :checks => people(rows).size >= min ? rows.size : nil,
            :periods => periods(rows, days, now, min),
            :schools => schools(rows, min),
            :activities => activities(rows, min),
            :stumbling => stumbling(rows, min),
        }
    end

    def self.people(rows) = rows.map { |r| r[:person] }.uniq

    # Per day (7 days) or per week (longer): students and checks.
    def self.periods(rows, days, now, min)
        step = days <= 7 ? 1 : 7
        start = Date.parse(Time.at(now).strftime('%Y-%m-%d')) - days + 1
        (0...days).step(step).map do |offset|
            from = start + offset
            to = from + step
            inside = rows.select { |r| d = Date.parse(Time.at(r[:ts]).strftime('%Y-%m-%d')); d >= from && d < to }
            n = people(inside).size
            { :from => from, :to => to - 1, :people => n >= min ? n : nil, :checks => n >= min ? inside.size : nil }
        end
    end

    def self.schools(rows, min)
        rows.group_by { |r| r[:school] }.map do |school, own|
            n = people(own).size
            entry = { :school => school, :people => n >= min ? n : nil }
            if n >= min
                entry[:checks] = own.size
                entry[:activities] = own.group_by { |r| r[:activity] }.map do |activity, part|
                    k = people(part).size
                    { :activity => activity, :people => k >= min ? k : nil, :checks => k >= min ? part.size : nil }
                end.sort_by { |a| [-(a[:people] || 0), a[:activity].to_s] }
            end
            entry
        end.sort_by { |s| [-(s[:people] || 0), s[:school].to_s] }
    end

    def self.activities(rows, min)
        rows.group_by { |r| r[:activity] }.map do |activity, own|
            n = people(own).size
            entry = { :activity => activity, :people => n >= min ? n : nil }
            if n >= min
                entry[:checks] = own.size
                entry[:schools] = own.map { |r| r[:school] }.uniq.size
                entry[:tasks] = own.group_by { |r| r[:task] }.map { |task, part| task_numbers(task, part, min) }
                                   .sort_by { |t| t[:task].to_s }
            end
            entry
        end.sort_by { |a| [-(a[:people] || 0), a[:activity].to_s] }
    end

    # One task: how many tried it and solved it, the share of wrong checks,
    # the median of checks up to the first correct one (in the window).
    def self.task_numbers(task, rows, min)
        tried = people(rows)
        return { :task => task, :people => nil } if tried.size < min

        per_person = rows.group_by { |r| r[:person] }.transform_values { |list| list.sort_by { |r| r[:ts].to_i } }
        until_correct = per_person.values.filter_map do |list|
            at = list.index { |r| r[:status] == 'correct' }
            at && at + 1
        end
        judged = rows.count { |r| %w[correct wrong].include?(r[:status]) }
        {
            :task => task,
            :people => tried.size,
            :checks => rows.size,
            :solved => until_correct.size.to_f / tried.size,
            :wrong => judged.zero? ? nil : rows.count { |r| r[:status] == 'wrong' }.to_f / judged,
            :median => until_correct.size >= min ? median(until_correct) : nil,
        }
    end

    def self.median(list)
        sorted = list.sort
        mid = sorted.size / 2
        sorted.size.odd? ? sorted[mid].to_f : (sorted[mid - 1] + sorted[mid]) / 2.0
    end

    # The tasks where most go wrong: the lowest share solved, then the most
    # checks until solved; only tasks enough students tried.
    def self.stumbling(rows, min)
        rows.group_by { |r| [r[:activity], r[:task]] }.filter_map do |(activity, task), part|
            numbers = task_numbers(task, part, min)
            next unless numbers[:people] && !numbers[:wrong].nil?

            numbers.merge(:activity => activity)
        end.sort_by { |t| [t[:solved], -(t[:median] || 0), -(t[:wrong] || 0)] }.first(STUMBLING)
    end

    # ----- the page -----

    def self.h(text) = CGI.escapeHTML(text.to_s)

    def self.number(n, min) = n.nil? ? "&lt; #{min}" : n.to_s

    def self.percent(share) = share.nil? ? '–' : "#{(share * 100).round} %"

    def self.decimal(value) = value.nil? ? '–' : format('%.1f', value).sub('.', ',')

    def self.date(d) = d.strftime('%-d.%-m.')

    # titles: { activity => [title, slug] }, tasks: { 'activity/id' => title },
    # schools: { key => name }
    def self.html(summary, titles:, tasks:, schools:)
        min = summary[:min]
        activity_link = lambda do |activity|
            title, slug = titles[activity] || [activity, nil]
            slug ? "<a href='/#{h(slug)}'>#{h(title)}</a>" : h(title)
        end
        task_name = ->(task) { h(tasks[task] || task.to_s.split('/', 2).last) }
        io = +''
        io << "<nav class='usage-windows mb-3' aria-label='Zeitraum'>"
        WINDOWS.each do |d|
            current = d == summary[:days]
            io << "<a class='btn btn-sm #{current ? 'btn-primary' : 'btn-outline-primary'} me-2' href='/statistik?tage=#{d}'#{current ? " aria-current='page'" : ''}>#{d} Tage</a>"
        end
        io << '</nav>'
        io << "<p>In den letzten #{summary[:days]} Tagen: <strong>#{number(summary[:people], min)}</strong> Lernende mit " \
              "<strong>#{number(summary[:checks], min)}</strong> Prüfungen.</p>"

        io << "<h2 class='mt-4'>Verlauf</h2>"
        io << "<div class='table-responsive'><table class='table table-sm usage-table'><thead><tr><th>#{summary[:days] <= 7 ? 'Tag' : 'Woche'}</th><th class='text-end'>Lernende</th><th class='text-end'>Prüfungen</th></tr></thead><tbody>"
        summary[:periods].each do |p|
            label = p[:from] == p[:to] ? date(p[:from]) : "#{date(p[:from])} bis #{date(p[:to])}"
            io << "<tr><td>#{label}</td><td class='text-end'>#{number(p[:people], min)}</td><td class='text-end'>#{number(p[:checks], min)}</td></tr>"
        end
        io << '</tbody></table></div>'

        io << "<h2 class='mt-4'>Stolpersteine</h2>"
        io << "<p>Aufgaben, die mindestens #{min} Lernende versucht haben, die mit dem kleinsten Anteil Gelöster zuerst. " \
              '»Bis gelöst« ist der Median der Prüfungen bis zur ersten richtigen, im Zeitraum.</p>'
        if summary[:stumbling].empty?
            io << "<p class='text-muted'>Noch keine Aufgabe mit genug Lernenden.</p>"
        else
            io << "<div class='table-responsive'><table class='table table-sm usage-table'><thead><tr><th>Aktivität</th><th>Aufgabe</th>" \
                  "<th class='text-end'>Lernende</th><th class='text-end'>gelöst</th><th class='text-end'>falsch</th><th class='text-end'>bis gelöst</th></tr></thead><tbody>"
            summary[:stumbling].each do |t|
                io << "<tr><td>#{activity_link.(t[:activity])}</td><td>#{task_name.(t[:task])}</td><td class='text-end'>#{t[:people]}</td>" \
                      "<td class='text-end'>#{percent(t[:solved])}</td><td class='text-end'>#{percent(t[:wrong])}</td><td class='text-end'>#{decimal(t[:median])}</td></tr>"
            end
            io << '</tbody></table></div>'
        end

        io << "<h2 class='mt-4'>Schulen</h2>"
        if summary[:schools].empty?
            io << "<p class='text-muted'>Keine Prüfungen in diesem Zeitraum.</p>"
        end
        summary[:schools].each do |s|
            name = schools[s[:school]] || 'ohne Schule'
            io << "<h3 class='h5 mt-3'>#{h(name)} <span class='text-muted fw-normal'>#{number(s[:people], min)} Lernende" \
                  "#{s[:checks] ? ", #{s[:checks]} Prüfungen" : ''}</span></h3>"
            next unless s[:activities]

            big, small = s[:activities].select { |a| titles.key?(a[:activity]) }.partition { |a| a[:people] }
            # not named: that someone at this school did it is all it would say
            rest = small.empty? ? '' : "#{small.size == 1 ? 'Eine Aktivität' : "#{small.size} Aktivitäten"} mit weniger als #{min} Lernenden."
            if big.empty?
                io << "<p class='text-muted'>#{rest}</p>"
                next
            end
            io << "<div class='table-responsive'><table class='table table-sm usage-table'><thead><tr><th>Aktivität</th><th class='text-end'>Lernende</th><th class='text-end'>Prüfungen</th></tr></thead><tbody>"
            big.each do |a|
                io << "<tr><td>#{activity_link.(a[:activity])}</td><td class='text-end'>#{a[:people]}</td><td class='text-end'>#{a[:checks]}</td></tr>"
            end
            io << "<tr><td class='text-muted' colspan='3'>#{rest}</td></tr>" unless rest.empty?
            io << '</tbody></table></div>'
        end

        io << "<h2 class='mt-4'>Aktivitäten</h2>"
        # those no longer in the catalog only count in the totals
        listed, small = summary[:activities].select { |a| titles.key?(a[:activity]) }.partition { |a| a[:people] }
        io << "<p class='text-muted'>Noch keine Aktivität mit mindestens #{min} Lernenden.</p>" if listed.empty?
        listed.each do |a|
            io << "<details class='usage-activity'><summary>#{activity_link.(a[:activity])} <span class='text-muted'>#{number(a[:people], min)} Lernende" \
                  "#{a[:schools] ? ", #{a[:schools] == 1 ? 'eine Schule' : "#{a[:schools]} Schulen"}" : ''}</span></summary>"
            if a[:tasks]
                io << "<div class='table-responsive'><table class='table table-sm usage-table'><thead><tr><th>Aufgabe</th><th class='text-end'>Lernende</th>" \
                      "<th class='text-end'>Prüfungen</th><th class='text-end'>gelöst</th><th class='text-end'>falsch</th><th class='text-end'>bis gelöst</th></tr></thead><tbody>"
                a[:tasks].each do |t|
                    if t[:people]
                        io << "<tr><td>#{task_name.(t[:task])}</td><td class='text-end'>#{t[:people]}</td><td class='text-end'>#{t[:checks]}</td>" \
                              "<td class='text-end'>#{percent(t[:solved])}</td><td class='text-end'>#{percent(t[:wrong])}</td><td class='text-end'>#{decimal(t[:median])}</td></tr>"
                    else
                        io << "<tr><td>#{task_name.(t[:task])}</td><td class='text-end'>&lt; #{min}</td><td></td><td></td><td></td><td></td></tr>"
                    end
                end
                io << '</tbody></table></div>'
            end
            io << '</details>'
        end
        unless small.empty?
            io << "<p class='mt-3'>Mit weniger als #{min} Lernenden: #{small.sort_by { |a| titles[a[:activity]][0] }.map { |a| activity_link.(a[:activity]) }.join(', ')}.</p>"
        end
        io
    end
end
