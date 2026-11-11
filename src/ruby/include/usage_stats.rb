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
    WINDOWS = [7, 30, 90, 365].freeze
    DEFAULT_WINDOW = 30
    STUMBLING = 15
    WEEKDAYS = %w[Mo Di Mi Do Fr Sa So].freeze
    MONTHS = %w[Jan Feb Mär Apr Mai Jun Jul Aug Sep Okt Nov Dez].freeze

    def self.day_of(ts) = Time.at(ts.to_i).to_date

    # The first day of the window: today and the days before it, for a
    # year from a Monday on, so that its weeks are calendar weeks.
    def self.first_day(days, now)
        first = day_of(now) - days + 1
        days > 90 ? first - (first.cwday - 1) : first
    end

    # rows: [{ person:, school:, groups:, activity:, task:, status:, ts: }],
    # person a number that stands for one student within this request,
    # groups the classes the student is in (no test groups); group_school:
    # { group => school }.
    def self.summary(rows, days:, now: Time.now.to_i, min: MIN_PEOPLE, group_school: {}, hours: [])
        first = first_day(days, now)
        since = Time.new(first.year, first.month, first.day).to_i
        rows = rows.select { |r| r[:ts].to_i >= since }
        count = ->(people) { people.size >= min ? people.size : nil }
        {
            :days => days,
            :min => min,
            :people => count.(people(rows)),
            :checks => people(rows).size >= min ? rows.size : nil,
            :periods => periods(rows, days, now, min),
            :rhythm => rhythm(rows, min),
            :schools => schools(rows, min, days, now),
            :activities => activities(rows, min),
            :stumbling => stumbling(rows, min, group_school),
            :classes => classes(rows, min, days, now, group_school),
            :today => day_of(now),
            :workspaces => workspace_periods(hours, days, now, min),
            :workspace_rhythm => workspace_rhythm(hours, min),
            :workspaces_since => hours.map { |h| h[:hour] }.min,
        }
    end

    def self.people(rows) = rows.map { |r| r[:person] }.uniq

    # A number of students as the page may show it: the number from min on,
    # :few for one to min - 1 (»weniger als fünf«), 0 for none.
    def self.shown(n, min) = n >= min ? n : (n.zero? ? 0 : :few)

    # Per day (up to 90 days) or per calendar week (a year): students and
    # checks.
    def self.periods(rows, days, now, min)
        step = days > 90 ? 7 : 1
        last = day_of(now)
        by_day = rows.group_by { |r| day_of(r[:ts]) }
        (first_day(days, now)..last).step(step).map do |from|
            to = [from + step - 1, last].min
            inside = (from..to).flat_map { |d| by_day[d] || [] }
            n = people(inside).size
            { :from => from, :to => to, :people => shown(n, min), :checks => n >= min ? inside.size : nil }
        end
    end

    # Running Workspaces (WorkspaceUsage) per day or calendar week: the
    # busiest hour of it (different Workspaces in that hour) and the most
    # at once; nil before the first counted hour.
    def self.workspace_periods(hours, days, now, min)
        step = days > 90 ? 7 : 1
        last = day_of(now)
        by_day = hours.group_by { |h| day_of(h[:hour]) }
        (first_day(days, now)..last).step(step).map do |from|
            to = [from + step - 1, last].min
            inside = (from..to).flat_map { |d| by_day[d] || [] }
            top = inside.map { |h| h[:running] }.max.to_i
            { :from => from, :to => to, :people => shown(top, min), :peak => inside.map { |h| h[:peak] }.max.to_i, :counted => !inside.empty? }
        end
    end

    # When in the week: running Workspaces per weekday and hour, the mean
    # over the counted hours of the window.
    def self.workspace_rhythm(hours, min)
        cells = hours.group_by { |h| t = Time.at(h[:hour]); [(t.wday + 6) % 7, t.hour] }
        (0..6).map do |wday|
            (0..23).map do |hour|
                list = cells[[wday, hour]] || []
                shown(list.empty? ? 0 : (list.sum { |h| h[:running] }.to_f / list.size).round, min)
            end
        end
    end

    # When in the week: students per weekday (Monday first) and hour.
    def self.rhythm(rows, min)
        cells = rows.group_by { |r| t = Time.at(r[:ts].to_i); [(t.wday + 6) % 7, t.hour] }
        (0..6).map { |wday| (0..23).map { |hour| shown(people(cells[[wday, hour]] || []).size, min) } }
    end

    def self.schools(rows, min, days, now)
        rows.group_by { |r| r[:school] }.map do |school, own|
            n = people(own).size
            entry = { :school => school, :people => n >= min ? n : nil }
            if n >= min
                entry[:checks] = own.size
                entry[:periods] = periods(own, days, now, min)
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
    def self.stumbling(rows, min, group_school = {})
        rows.group_by { |r| [r[:activity], r[:task]] }.filter_map do |(activity, task), part|
            numbers = task_numbers(task, part, min)
            next unless numbers[:people] && !numbers[:wrong].nil?

            numbers.merge(:activity => activity)
        end.sort_by { |t| [t[:solved], -(t[:median] || 0), -(t[:wrong] || 0)] }.first(STUMBLING).map do |t|
            t.merge(places(rows.select { |r| r[:task] == t[:task] && r[:activity] == t[:activity] }, t[:task], min, group_school))
        end
    end

    # The classes that worked in the window, the most recent first: each in
    # which at least min different students checked something, with its
    # school, its days (or weeks), the last of them with min of its students
    # (a lesson, most of the time) and what most of them worked on then, if
    # min did; the smaller classes only counted.
    def self.classes(rows, min, days, now, group_school)
        by_group = Hash.new { |hash, key| hash[key] = [] }
        rows.each { |r| (r[:groups] || []).each { |g| by_group[g] << r } }
        named, small = by_group.partition { |_, part| people(part).size >= min }
        list = named.map do |group, part|
            own = periods(part, days, now, min)
            lesson = own.reverse.find { |p| p[:people].is_a?(Integer) && p[:people].positive? }
            topic = nil
            if lesson
                inside = part.select { |r| (lesson[:from]..lesson[:to]).cover?(day_of(r[:ts])) }
                activity, its = inside.group_by { |r| r[:activity] }.max_by { |_, x| people(x).size }
                topic = activity if its && people(its).size >= min
            end
            { :group => group, :school => group_school[group], :people => people(part).size, :periods => own,
              :last => lesson && lesson[:from], :last_period => lesson, :activity => topic }
        end
        list.sort_by! { |c| [c[:last] ? -c[:last].jd : 0, -c[:people], c[:group].to_s] }
        { :list => list, :small => small.size }
    end

    # Where a task goes wrong: each class in which at least min students
    # tried it, with its own share solved, the lowest first; a school none
    # of whose classes has min, but which has min itself, as the school
    # (only then, so that a class and its school never give away the rest
    # by difference); and how many classes are too small to be named.
    def self.places(rows, task, min, group_school)
        by_group = Hash.new { |hash, key| hash[key] = [] }
        rows.each { |r| (r[:groups] || []).each { |g| by_group[g] << r } }
        named, small = by_group.partition { |_, part| people(part).size >= min }
        list = named.map do |group, part|
            numbers = task_numbers(task, part, min)
            { :group => group, :school => group_school[group], :people => numbers[:people], :solved => numbers[:solved] }
        end
        covered = list.map { |p| p[:school] }.uniq
        rows.group_by { |r| r[:school] }.each do |school, part|
            next if covered.include?(school) || people(part).size < min

            numbers = task_numbers(task, part, min)
            list << { :group => nil, :school => school, :people => numbers[:people], :solved => numbers[:solved] }
        end
        list.sort_by! { |p| [p[:solved], -p[:people], p[:group].to_s] }
        { :places => list, :small_places => small.size }
    end

    # ----- the page -----

    def self.h(text) = CGI.escapeHTML(text.to_s)

    def self.number(n, min) = n.nil? ? "&lt; #{min}" : thousands(n)

    def self.thousands(n) = n.to_s.reverse.scan(/\d{1,3}/).join('.').reverse

    def self.percent(share) = share.nil? ? '–' : "#{(share * 100).round} %"

    def self.decimal(value) = value.nil? ? '–' : format('%.1f', value).sub('.', ',')

    def self.date(d) = d.strftime('%-d.%-m.')

    def self.period_label(p)
        return "#{WEEKDAYS[p[:from].cwday - 1]} #{date(p[:from])}" if p[:from] == p[:to]

        "#{date(p[:from])} bis #{date(p[:to])}"
    end

    # Grid lines: steps of 1, 2 or 5 times a power of ten, at most four.
    def self.ticks(max)
        max = [max, 1].max
        k = 1
        loop do
            [1, 2, 5].each do |f|
                step = f * k
                return (0..(max.to_f / step).ceil).map { |i| i * step } if max.to_f / step <= 4
            end
            k *= 10
        end
    end

    # Where a label goes under the bars: every day of a week, the Mondays
    # of a month, the first day (or week) of each month beyond that.
    def self.axis_labels(periods)
        periods.each_with_index.filter_map do |p, i|
            d = p[:from]
            text = if periods.size <= 7 then "#{WEEKDAYS[d.cwday - 1]} #{d.day}."
                   elsif periods.size <= 31 then (d.monday? ? date(d) : nil)
                   elsif i.positive? && d.month != periods[i - 1][:from].month then MONTHS[d.month - 1]
                   end
            text && [i, text]
        end
    end

    # A bar per day or week, as high as the number of students; one to
    # four students a grey stub, so that a quiet day differs from none.
    # small: a strip without axes (a school), with its own scale.
    # tip: the words of a bar, ->(period, n) for n from min on; few and none
    # for the grey stub and an empty day
    STUDENT_TIP = ->(p, n) { "#{n} Lernende, #{p[:checks]} Prüfungen" }

    def self.bars(periods, min, small: false, tip: STUDENT_TIP, few: 'Lernende', none: 'keine Prüfung', axis: true)
        max = periods.map { |p| p[:people].is_a?(Integer) ? p[:people] : 0 }.max.to_i
        top = small ? [max, min].max : ticks([max, min].max).last
        weekly = periods.any? { |p| p[:from] != p[:to] }
        io = +"<div class='usage-plot#{small ? ' usage-plot-small' : ''}'>"
        unless small
            ticks([max, min].max).each do |t|
                io << "<span class='usage-tick' style='bottom: #{(100.0 * t / top).round(2)}%'><span>#{t}</span></span>"
            end
        end
        io << "<div class='usage-bars#{periods.size > 60 ? ' usage-bars-dense' : ''}'>"
        periods.each do |p|
            n = p[:people]
            weekend = !weekly && (p[:from].saturday? || p[:from].sunday?)
            tip_text, fill = if n == :few then ["#{period_label(p)}: weniger als #{min} #{few}", "<i class='usage-fill usage-few'></i>"]
                        elsif n.zero? then ["#{period_label(p)}: #{none.respond_to?(:call) ? none.(p) : none}", '']
                        else ["#{period_label(p)}: #{tip.(p, n)}",
                              "<i class='usage-fill' style='height: #{(100.0 * n / top).round(2)}%'></i>"]
                        end
            io << "<span class='usage-col#{weekend ? ' usage-weekend' : ''}' data-tip='#{h(tip_text)}'>#{fill}</span>"
        end
        io << '</div></div>'
        if !small && axis
            io << "<div class='usage-axis' aria-hidden='true'>"
            axis_labels(periods).each do |i, text|
                io << "<span style='left: #{(100.0 * i / periods.size).round(2)}%'>#{h(text)}</span>"
            end
            io << '</div>'
        end
        io
    end

    # Weekdays by hours, darker where more students checked something.
    def self.rhythm_html(rhythm, min, noun: 'Lernende', none: 'keine Prüfung')
        max = rhythm.flatten.grep(Integer).max.to_i
        io = +"<div class='usage-rhythm' role='group' aria-label='#{noun} nach Wochentag und Uhrzeit'><span></span>"
        24.times { |hour| io << "<span class='usage-hour'>#{hour % 3 == 0 ? hour : ''}</span>" }
        rhythm.each_with_index do |row, wday|
            io << "<span class='usage-day'>#{WEEKDAYS[wday]}</span>"
            row.each_with_index do |n, hour|
                when_ = "#{WEEKDAYS[wday]} #{hour} bis #{hour + 1} Uhr"
                if n == :few
                    io << "<span class='usage-cell usage-few' data-tip='#{when_}: weniger als #{min} #{noun}'></span>"
                elsif n.zero?
                    io << "<span class='usage-cell' data-tip='#{when_}: #{none}'></span>"
                else
                    level = (5.0 * n / max).ceil.clamp(1, 5)
                    io << "<span class='usage-cell usage-level-#{level}' data-tip='#{when_}: #{n} #{noun}'></span>"
                end
            end
        end
        io << '</div>'
        io << "<p class='usage-legend'>weniger #{(1..5).map { |l| "<i class='usage-swatch usage-level-#{l}'></i>" }.join} mehr" \
              "<span class='ms-3'><i class='usage-swatch usage-few'></i> weniger als #{min}</span></p>"
        io
    end


    # titles: { activity => [title, slug] }, tasks: { 'activity/id' => title },
    # schools: { key => name }
    PLACES = 4

    # A share as a small pie: blue for solved, coral for wrong checks; the
    # number under the pointer.
    def self.pie(share, kind, tip)
        return "<span class='usage-pie usage-pie-none' data-tip='#{h(tip)}: –'></span>" if share.nil?

        "<span class='usage-pie usage-pie-#{kind}' style='--p: #{(share * 100).round(1)}%' role='img' " \
            "aria-label='#{h(tip)}: #{percent(share)}' data-tip='#{h(tip)}: #{percent(share)}'></span>"
    end

    # A bar of fixed length with its number beside it, so that bars and
    # numbers line up from row to row.
    def self.hbar(value, max)
        width = max.to_i.positive? ? (100.0 * value / max).round(1) : 0
        "<span class='usage-hbar'><span class='usage-hbar-track'><i style='width: #{width}%'></i></span>" \
            "<span class='usage-hbar-number'>#{value}</span></span>"
    end

    def self.median_text(value) = value.nil? ? '–' : decimal(value).sub(/,0\z/, '')

    def self.task_header(first) = "<thead><tr><th>#{first}</th><th class='text-end'>Lernende</th>" \
        "<th class='usage-pie-cell'>gelöst</th><th class='usage-pie-cell'>falsch</th><th class='text-end text-nowrap'>bis gelöst</th></tr></thead>"

    def self.task_cells(t) = "<td class='text-end usage-num' data-label='Lernende'>#{t[:people]}</td>" \
        "<td class='usage-pie-cell' data-label='gelöst'>#{pie(t[:solved], 'solved', 'gelöst')}</td>" \
        "<td class='usage-pie-cell' data-label='falsch'>#{pie(t[:wrong], 'wrong', 'Prüfungen falsch')}</td>" \
        "<td class='text-end usage-num' data-label='bis gelöst'>#{median_text(t[:median])}</td>"

    # A day as people say it: heute, gestern, vor 3 Tagen, then the date
    def self.recent(day, today)
        ago = (today - day).to_i
        return 'heute' if ago <= 0
        return 'gestern' if ago == 1
        return "vor #{ago} Tagen" if ago < 7

        "#{WEEKDAYS[day.cwday - 1]} #{date(day)}"
    end

    # One row of the stacked course: a title, a note, the chart
    def self.strip(title, note, chart)
        "<section class='usage-stack-row'><div class='usage-stack-head'><h3>#{h(title)}</h3>" \
            "<span>#{h(note)}</span></div>#{chart}</section>"
    end

    # The course in time: students, running Workspaces and the server, one
    # strip each, on the same days, the dates only under the last, so that
    # a busy day lines up across them (each strip keeps its own scale).
    def self.course_html(summary, server)
        min = summary[:min]
        unit = summary[:days] > 90 ? 'Woche' : 'Tag'
        rows = [['Lernende', "die etwas prüfen ließen, je #{unit}",
                 ->(axis) { bars(summary[:periods], min, :axis => axis) }]]
        unless summary[:workspaces_since].nil?
            rows << ['Laufende Workspaces', "in der vollsten Stunde, auch von Lehrkräften, je #{unit}",
                     lambda do |axis|
                         bars(summary[:workspaces], min, :axis => axis,
                              :tip => ->(p, n) { "höchstens #{n} Workspaces in einer Stunde, #{p[:peak]} gleichzeitig" },
                              :few => 'Workspaces', :none => (->(p) { p[:counted] ? 'kein Workspace' : 'nicht gezählt' }))
                     end]
        end
        if server && server[:since]
            labels = axis_labels(server[:periods])
            ServerLoad::CHARTS.first(2).each do |title, note, value, tip|
                rows << [title, note, ->(axis) { ServerLoad.plot(ServerLoad.columns(server[:periods], server[:today], value, tip), axis ? labels : nil) }]
            end
        end
        io = +"<div class='usage-stack'>"
        rows.each_with_index { |(title, note, chart), i| io << strip(title, note, chart.(i == rows.size - 1)) }
        if summary[:workspaces_since].nil?
            io << strip('Laufende Workspaces', 'noch keine Zahlen',
                        "<p class='usage-legend'>Noch keine Zahlen. Der Workspace zählt alle fünf Minuten, wie viele Workspaces laufen, " \
                        'und speichert je Stunde nur zwei Zahlen, nicht wer.</p>')
        end
        io << '</div>'
        io << "<p class='usage-legend'>Wochenenden grau hinterlegt; ein grauer Strich: weniger als #{min}. " \
              "#{server && server[:since] && unit == 'Tag' ? "Ein Klick auf einen Tag der letzten #{ServerLoad::SAMPLE_DAYS} Tage beim Server zeigt ihn in Fünf-Minuten-Schritten." : ''}</p>"
        # the same as a table
        io << "<details class='usage-table-view'><summary>Als Tabelle</summary><div class='table-responsive'><table class='table table-sm usage-table'>"
        io << "<thead><tr><th>#{unit}</th><th class='text-end'>Lernende</th><th class='text-end'>Prüfungen</th>"
        io << "<th class='text-end'>Workspaces</th>" unless summary[:workspaces_since].nil?
        io << "<th class='text-end'>Prozessoren</th><th class='text-end'>Arbeitsspeicher</th>" if server && server[:since]
        io << '</tr></thead><tbody>'
        pct = ->(v) { v.nil? ? '–' : "#{decimal(v)} %" }
        summary[:periods].each_with_index do |p, i|
            people = p[:people] == :few ? nil : p[:people]
            io << "<tr><td>#{period_label(p)}</td><td class='text-end'>#{number(people, min)}</td>" \
                  "<td class='text-end'>#{p[:people] == 0 ? 0 : number(p[:checks], min)}</td>"
            unless summary[:workspaces_since].nil?
                w = summary[:workspaces][i]
                io << "<td class='text-end'>#{w.nil? || !w[:counted] ? '–' : (w[:people] == :few ? "&lt; #{min}" : w[:people])}</td>"
            end
            if server && server[:since]
                sp = server[:periods][i]
                io << "<td class='text-end'>#{pct.(sp && sp[:cpu_hour])}</td><td class='text-end'>#{pct.(sp && ServerLoad.share(sp[:ram]))}</td>"
            end
            io << '</tr>'
        end
        io << '</tbody></table></div></details>'
    end

    # A class (or a school) on »Stolpersteine«: its name with a small pie of
    # its share solved; its school and the numbers under the pointer, so
    # that several classes fit on one line
    def self.place_chip(pl, school_name)
        name = h(pl[:group] || school_name.(pl[:school]))
        label = pl[:group] ? "#{pl[:group]}, #{school_name.(pl[:school])}" : school_name.(pl[:school])
        "<span class='usage-place' data-tip='#{h(label)}: #{pl[:people]} Lernende, #{percent(pl[:solved])} gelöst'>" \
            "<span class='usage-pie usage-pie-solved usage-pie-small' style='--p: #{(pl[:solved] * 100).round(1)}%'></span>#{name}</span>"
    end

    STUMBLING_SHOWN = 8

    # titles: { activity => [title, slug] }, tasks: { 'activity/id' => title },
    # schools: { key => name }; server: { periods:, rhythm:, since:, today: }
    # of the server (ServerLoad), nil without
    def self.html(summary, titles:, tasks:, schools:, server: nil)
        min = summary[:min]
        activity_link = lambda do |activity|
            title, slug = titles[activity] || [activity, nil]
            slug ? "<a href='/#{h(slug)}'>#{h(title)}</a>" : h(title)
        end
        task_name = ->(task) { h(tasks[task] || task.to_s.split('/', 2).last) }
        school_name = ->(key) { schools[key] || 'ohne Schule' }
        unit = summary[:days] > 90 ? 'Woche' : 'Tag'
        classes = summary[:classes] || { :list => [], :small => 0 }
        io = +"<div class='usage-charts'>"

        io << "<nav class='usage-windows mb-3' aria-label='Zeitraum'>"
        WINDOWS.each do |d|
            current = d == summary[:days]
            io << "<a class='btn btn-sm #{current ? 'btn-primary' : 'btn-outline-primary'} me-2' href='/statistik?tage=#{d}'#{current ? " aria-current='page'" : ''}>#{d == 365 ? 'ein Jahr' : "#{d} Tage"}</a>"
        end
        io << '</nav>'
        shown_schools = summary[:schools].count { |s| s[:people] }
        busiest = summary[:periods].map { |p| p[:people].is_a?(Integer) ? p[:people] : 0 }.max.to_i
        tiles = [[number(summary[:people], min), 'Lernende'], [number(summary[:checks], min), 'Prüfungen'],
                 [classes[:list].size.to_s, "#{classes[:list].size == 1 ? 'Klasse' : 'Klassen'} ab #{min} Lernenden"],
                 [shown_schools.to_s, "#{shown_schools == 1 ? 'Schule' : 'Schulen'} ab #{min} Lernenden"],
                 [busiest.positive? ? busiest.to_s : '–', unit == 'Tag' ? 'höchstens an einem Tag' : 'höchstens in einer Woche']]
        io << "<div class='usage-tiles' aria-label='#{summary[:days] == 365 ? 'Im letzten Jahr' : "In den letzten #{summary[:days]} Tagen"}'>"
        tiles.each { |value, label| io << "<div class='usage-tile'><span class='usage-tile-value'>#{value}</span><span class='usage-tile-label'>#{label}</span></div>" }
        io << '</div>'

        io << "<h2 class='mt-4'>Verlauf</h2>"
        io << "<p>Wer arbeitet und was der Server dabei leistet, #{unit == 'Tag' ? 'an jedem Tag' : 'in jeder Woche'}, untereinander auf denselben " \
              "#{unit == 'Tag' ? 'Tagen' : 'Wochen'}: Ein voller Tag ist in allen Streifen an derselben Stelle. Jeder Streifen hat seinen eigenen Maßstab.</p>"
        io << course_html(summary, server)

        io << "<h2 class='mt-4'>Wochenrhythmus</h2>"
        io << '<p>Wann in der Woche gearbeitet wird, je Wochentag und Stunde über den ganzen Zeitraum, nebeneinander.</p>'
        io << "<div class='usage-rhythms'>"
        io << "<section><h3>Lernende</h3>#{rhythm_html(summary[:rhythm], min)}</section>"
        unless summary[:workspaces_since].nil?
            io << "<section><h3>Laufende Workspaces</h3>#{rhythm_html(summary[:workspace_rhythm], min, :noun => 'Workspaces', :none => 'kein Workspace')}</section>"
        end
        io << "<section><h3>Prozessoren</h3>#{ServerLoad.rhythm_html(server[:rhythm])}</section>" if server && server[:since]
        io << '</div>'

        io << "<h2 class='mt-4'>Klassen</h2>"
        io << "<p>Klassen, in denen mindestens #{min} Lernende etwas prüfen ließen, die zuletzt aktiven zuerst. " \
              "»Zuletzt« ist #{unit == 'Tag' ? 'der letzte Tag' : 'die letzte Woche'}, an #{unit == 'Tag' ? 'dem' : 'der'} mindestens #{min} aus der Klasse gearbeitet haben, " \
              'meist eine Stunde im Unterricht, und »woran« das, woran die meisten von ihnen da gearbeitet haben.</p>'
        if classes[:list].empty?
            io << "<p class='text-muted'>Noch keine Klasse mit mindestens #{min} Lernenden.</p>"
        else
            io << "<div class='table-responsive'><table class='table table-sm usage-table usage-classes'><thead><tr><th>Klasse</th>" \
                  "<th class='text-end'>Lernende</th><th>zuletzt</th><th>woran</th><th class='usage-classes-strip'>#{unit == 'Tag' ? 'Tage' : 'Wochen'}</th></tr></thead><tbody>"
            classes[:list].each do |c|
                last = if c[:last_period].nil? then '–'
                       elsif unit == 'Tag' then recent(c[:last], summary[:today])
                       else "Woche ab #{date(c[:last])}"
                       end
                io << "<tr><td><span class='usage-class-name'>#{h(c[:group])}</span> <span class='usage-place-school'>#{h(school_name.(c[:school]))}</span></td>" \
                      "<td class='text-end usage-num'>#{c[:people]}</td><td class='text-nowrap'>#{last}</td>" \
                      "<td>#{c[:activity] ? activity_link.(c[:activity]) : '–'}</td>" \
                      "<td class='usage-classes-strip'>#{bars(c[:periods], min, :small => true)}</td></tr>"
            end
            io << '</tbody></table></div>'
        end
        io << "<p class='text-muted'>#{classes[:small]} #{classes[:small] == 1 ? 'Klasse' : 'Klassen'} mit weniger als #{min}.</p>" if classes[:small].to_i.positive?

        io << "<h2 class='mt-4'>Stolpersteine</h2>"
        io << "<p>Aufgaben, die mindestens #{min} Lernende versucht haben, die mit dem kleinsten Anteil Gelöster zuerst: " \
              "<span class='usage-key'>#{pie(0.7, 'solved', 'Beispiel')}</span> wie viele sie gelöst haben, " \
              "<span class='usage-key'>#{pie(0.3, 'wrong', 'Beispiel')}</span> wie viele Prüfungen falsch waren, " \
              "»bis gelöst« der Median der Prüfungen bis zur ersten richtigen; rechts die Klassen ab #{min}, die sie versucht haben.</p>"
        if summary[:stumbling].empty?
            io << "<p class='text-muted'>Noch keine Aufgabe mit genug Lernenden.</p>"
        else
            head = "<thead><tr><th>Aufgabe</th><th class='text-end'>Lernende</th><th class='usage-pie-cell'>gelöst</th>" \
                   "<th class='usage-pie-cell'>falsch</th><th class='text-end text-nowrap'>bis gelöst</th><th>Klassen</th></tr></thead>"
            row = lambda do |t|
                places = t[:places] || []
                chips = places.first(PLACES).map { |pl| place_chip(pl, school_name) }.join
                hidden = places.drop(PLACES)
                rest = []
                rest << "+#{hidden.size}" unless hidden.empty?
                rest << "#{t[:small_places]} unter #{min}" if t[:small_places].to_i.positive?
                chips << "<span class='usage-place-rest'>#{rest.join(', ')}</span>" unless rest.empty?
                "<tr><td><span class='usage-task-name'>#{task_name.(t[:task])}</span> <span class='usage-task-activity'>#{activity_link.(t[:activity])}</span></td>" \
                    "#{task_cells(t)}<td class='usage-places-cell'><div class='usage-places'>#{chips}</div></td></tr>"
            end
            first, more = summary[:stumbling].first(STUMBLING_SHOWN), summary[:stumbling].drop(STUMBLING_SHOWN)
            io << "<div class='table-responsive'><table class='table table-sm usage-table usage-tasks usage-stumbling'>#{head}<tbody>"
            first.each { |t| io << row.(t) }
            io << '</tbody></table></div>'
            unless more.empty?
                io << "<details class='usage-more'><summary>#{more.size} weitere #{more.size == 1 ? 'Aufgabe' : 'Aufgaben'}</summary>" \
                      "<div class='table-responsive'><table class='table table-sm usage-table usage-tasks usage-stumbling'>#{head}<tbody>"
                more.each { |t| io << row.(t) }
                io << '</tbody></table></div></details>'
            end
        end

        io << "<h2 class='mt-4'>Schulen</h2>"
        io << "<p class='text-muted'>Keine Prüfungen in diesem Zeitraum.</p>" if summary[:schools].empty?
        summary[:schools].each do |s|
            io << "<section class='usage-school'><h3 class='h5'>#{h(school_name.(s[:school]))} <span class='text-muted fw-normal'>#{number(s[:people], min)} Lernende" \
                  "#{s[:checks] ? ", #{thousands(s[:checks])} Prüfungen" : ''}</span></h3>"
            unless s[:activities]
                io << '</section>'
                next
            end

            peak = s[:periods].map { |p| p[:people].is_a?(Integer) ? p[:people] : 0 }.max
            io << "<div class='usage-school-strip'>#{bars(s[:periods], min, :small => true)}" \
                  "<span class='usage-peak'>#{peak.positive? ? "höchstens #{peak}<br>#{unit == 'Tag' ? 'am Tag' : 'in einer Woche'}" : ''}</span></div>"
            big, small = s[:activities].select { |a| titles.key?(a[:activity]) }.partition { |a| a[:people] }
            # not named: that someone at this school did it is all it would say
            rest = small.empty? ? '' : "#{small.size == 1 ? 'Eine Aktivität' : "#{small.size} Aktivitäten"} mit weniger als #{min} Lernenden."
            if big.empty?
                io << "<p class='text-muted'>#{rest}</p></section>"
                next
            end
            io << "<div class='table-responsive'><table class='table table-sm usage-table'><thead><tr><th>Aktivität</th><th>Lernende</th><th class='text-end'>Prüfungen</th></tr></thead><tbody>"
            big.each do |a|
                io << "<tr><td>#{activity_link.(a[:activity])}</td><td class='usage-hbar-cell'>#{hbar(a[:people], s[:people])}</td><td class='text-end usage-num'>#{thousands(a[:checks])}</td></tr>"
            end
            io << "<tr><td class='text-muted' colspan='3'>#{rest}</td></tr>" unless rest.empty?
            io << '</tbody></table></div></section>'
        end

        io << "<h2 class='mt-4'>Aktivitäten</h2>"
        # those no longer in the catalog only count in the totals
        listed, small = summary[:activities].select { |a| titles.key?(a[:activity]) }.partition { |a| a[:people] }
        io << "<p class='text-muted'>Noch keine Aktivität mit mindestens #{min} Lernenden.</p>" if listed.empty?
        most = listed.map { |a| a[:people] }.max.to_i
        listed.each do |a|
            io << "<details class='usage-activity'><summary><span class='usage-activity-title'>#{activity_link.(a[:activity])}</span>" \
                  "#{hbar(a[:people], most)}<span class='usage-activity-schools'>#{a[:schools] == 1 ? 'eine Schule' : "#{a[:schools]} Schulen"}</span></summary>"
            if a[:tasks]
                io << "<div class='table-responsive'><table class='table table-sm usage-table usage-tasks'>#{task_header('Aufgabe')}<tbody>"
                a[:tasks].each do |t|
                    if t[:people]
                        io << "<tr><td>#{task_name.(t[:task])}</td>#{task_cells(t)}</tr>"
                    else
                        io << "<tr><td>#{task_name.(t[:task])}</td><td class='text-end text-muted' data-label='Lernende'>&lt; #{min}</td><td></td><td></td><td></td></tr>"
                    end
                end
                io << '</tbody></table></div>'
            end
            io << '</details>'
        end
        unless small.empty?
            io << "<details class='usage-more mt-3'><summary>#{small.size} #{small.size == 1 ? 'Aktivität' : 'Aktivitäten'} mit weniger als #{min} Lernenden</summary>" \
                  "<p>#{small.sort_by { |a| titles[a[:activity]][0] }.map { |a| activity_link.(a[:activity]) }.join(', ')}.</p></details>"
        end
        io << '</div>'
        io
    end
end
