# The server's load for /statistik (»Server«): every few minutes, beside
# the count of running Workspaces (workspace_usage.rb), the server reads how
# busy the processors were since the last look (/proc/stat), how much memory
# is in use (/proc/meminfo) and how full the two disks are (df: the system's
# and /user, the Workspaces'). The Ruby container sees the whole machine in
# all three. Per hour the database keeps (ServerHour): the processors' mean
# (a sum and the number of looks, so that a restart in the middle of the
# hour adds instead of starting over), their busiest five minutes, the most
# memory in use, and the disks at the last look. Each look itself is kept
# too (ServerSample), for SAMPLE_DAYS, to see one day in five-minute steps;
# the hours stay without an end (about 9000 small nodes a year), since
# nothing in them is about people. Plain Ruby, tested on its own
# (test/server_load_test.rb); the server's part is in usage_stats_page.rb.
require 'cgi'
require 'date'
require_relative 'usage_stats'

module ServerLoad
    SAMPLE_DAYS = 90
    DISKS = { :root => '/', :user => '/user' }.freeze

    # /proc/stat's first line: [busy, total] in ticks since the start;
    # waiting for the disk (iowait) counts as idle
    def self.cpu_times(stat)
        values = stat.lines.first.split[1..].map(&:to_i)
        total = values.first(8).sum
        [total - values[3] - values[4].to_i, total]
    end

    # The processors' share in percent between two readings of cpu_times;
    # nil without a reading before or when no time passed
    def self.cpu_percent(before, after)
        return nil if before.nil? || after.nil?

        total = after[1] - before[1]
        return nil unless total.positive?

        (100.0 * (after[0] - before[0]) / total).clamp(0.0, 100.0).round(1)
    end

    # /proc/meminfo: [used, total] in bytes, used being what is not available
    def self.memory(meminfo)
        kb = ->(key) { meminfo[/^#{key}:\s+(\d+)/, 1].to_i * 1024 }
        total = kb.('MemTotal')
        [total - kb.('MemAvailable'), total]
    end

    # df -kP: { mount => [used, total] } in bytes, used being what is not
    # available (as on the admin page, the reserved blocks count as used)
    def self.disks(df)
        df.lines.drop(1).to_h do |line|
            parts = line.split
            total = parts[1].to_i * 1024
            [parts[5], [total - parts[3].to_i * 1024, total]]
        end
    end

    # Reads the machine; keeps the last /proc/stat to measure against.
    class Reader
        def initialize(stat: -> { File.read('/proc/stat') }, meminfo: -> { File.read('/proc/meminfo') },
                       df: -> { `df -kP #{DISKS.values.join(' ')} 2>/dev/null` })
            @stat = stat
            @meminfo = meminfo
            @df = df
            @last = nil
        end

        # { cpu: percent or nil, ram_used:, ram_total:, root_used:,
        #   root_total:, user_used:, user_total: } (disks nil when df fails)
        def read
            now = ServerLoad.cpu_times(@stat.call)
            cpu = ServerLoad.cpu_percent(@last, now)
            @last = now
            ram_used, ram_total = ServerLoad.memory(@meminfo.call)
            disks = ServerLoad.disks(@df.call)
            sample = { :cpu => cpu, :ram_used => ram_used, :ram_total => ram_total }
            DISKS.each do |key, mount|
                used, total = disks[mount]
                sample[:"#{key}_used"] = used
                sample[:"#{key}_total"] = total
            end
            sample
        end
    end

    # The Cypher that adds a sample to its hour: the mean as a sum and a
    # count, the maxima kept, the disks overwritten. Only what the sample
    # has (no processors on the first look after a start, no disks when df
    # failed).
    def self.store_query(sample)
        sets = ['h.ram_max = CASE WHEN coalesce(h.ram_max, 0) < $ram_used THEN $ram_used ELSE h.ram_max END',
                'h.ram_total = $ram_total']
        unless sample[:cpu].nil?
            sets << 'h.cpu_sum = coalesce(h.cpu_sum, 0) + $cpu'
            sets << 'h.cpu_samples = coalesce(h.cpu_samples, 0) + 1'
            sets << 'h.cpu_max = CASE WHEN coalesce(h.cpu_max, 0) < $cpu THEN $cpu ELSE h.cpu_max END'
        end
        DISKS.each_key do |key|
            next if sample[:"#{key}_total"].nil?

            sets << "h.#{key}_used = $#{key}_used"
            sets << "h.#{key}_total = $#{key}_total"
        end
        "MERGE (h:ServerHour {hour: $hour})\nSET #{sets.join(",\n    ")};"
    end

    # The look itself, with what it has
    def self.sample_query(sample)
        keys = [:ram_used, :ram_total] + (sample[:cpu].nil? ? [] : [:cpu]) +
               DISKS.keys.flat_map { |key| sample[:"#{key}_total"].nil? ? [] : [:"#{key}_used", :"#{key}_total"] }
        "CREATE (:ServerSample {ts: $ts, #{keys.map { |k| "#{k}: $#{k}" }.join(', ')}});"
    end

    # ---- the page ----------------------------------------------------

    def self.h(text) = CGI.escapeHTML(text.to_s)

    def self.decimal(value) = format('%.1f', value).sub('.', ',')

    def self.gb(bytes) = "#{decimal(bytes / 1e9)} GB"

    def self.share(pair) = pair && pair[1].to_i.positive? ? (100.0 * pair[0] / pair[1]).round(1) : nil

    # hours: [{ hour:, cpu_sum:, cpu_samples:, cpu_max:, ram_max:,
    # ram_total:, root_used:, root_total:, user_used:, user_total: }], a
    # value nil where the hour has none. Per day (up to 90 days) or calendar
    # week, as the other charts of /statistik: the processors' mean, their
    # busiest hour and five minutes, the most memory, the disks at the last
    # look.
    def self.periods(hours, days, now)
        step = days > 90 ? 7 : 1
        last = UsageStats.day_of(now)
        by_day = hours.group_by { |hr| UsageStats.day_of(hr[:hour]) }
        (UsageStats.first_day(days, now)..last).step(step).map do |from|
            to = [from + step - 1, last].min
            inside = (from..to).flat_map { |d| by_day[d] || [] }
            cpu = inside.select { |hr| hr[:cpu_samples].to_i.positive? }
            samples = cpu.sum { |hr| hr[:cpu_samples] }
            ram = inside.select { |hr| hr[:ram_total].to_i.positive? }.max_by { |hr| hr[:ram_max].to_f / hr[:ram_total] }
            disk = ->(key) { (hr = inside.reverse.find { |x| x[:"#{key}_total"].to_i.positive? }) && [hr[:"#{key}_used"], hr[:"#{key}_total"]] }
            { :from => from, :to => to, :counted => !inside.empty?,
              :cpu_mean => samples.positive? ? (cpu.sum { |hr| hr[:cpu_sum] } / samples).round(1) : nil,
              :cpu_hour => cpu.map { |hr| hr[:cpu_sum] / hr[:cpu_samples] }.max&.round(1),
              :cpu_max => cpu.map { |hr| hr[:cpu_max] }.compact.max,
              :ram => ram && [ram[:ram_max], ram[:ram_total]], :root => disk.(:root), :user => disk.(:user) }
        end
    end

    # The processors per weekday (Monday first) and hour: the mean of the
    # hours' means over the window, nil where nothing was measured.
    def self.rhythm(hours)
        cells = hours.select { |hr| hr[:cpu_samples].to_i.positive? }.group_by { |hr| t = Time.at(hr[:hour]); [(t.wday + 6) % 7, t.hour] }
        (0..6).map do |wday|
            (0..23).map do |hour|
                list = cells[[wday, hour]]
                list && (list.sum { |hr| hr[:cpu_sum] / hr[:cpu_samples] } / list.size).round(1)
            end
        end
    end

    # A chart in percent, 0 to 100: columns [{ value:, tip:, href: }],
    # labels [[index, text]] under it, none without (nil: no axis, as in
    # the stacked course of /statistik, where only the last strip has one).
    def self.plot(columns, labels, fine: false)
        io = +"<div class='usage-plot server-plot'>"
        [0, 50, 100].each { |t| io << "<span class='usage-tick' style='bottom: #{t}%'><span>#{t} %</span></span>" }
        io << "<div class='usage-bars#{fine ? ' usage-bars-fine' : (columns.size > 60 ? ' usage-bars-dense' : '')}'>"
        columns.each do |col|
            fill = col[:value].nil? ? '' : "<i class='usage-fill' style='height: #{col[:value].clamp(0, 100)}%'></i>"
            tag = col[:href] ? "a href='#{h(col[:href])}'" : 'span'
            io << "<#{tag} class='usage-col#{col[:weekend] ? ' usage-weekend' : ''}' data-tip='#{h(col[:tip])}'>#{fill}</#{tag.split.first}>"
        end
        io << '</div></div>'
        return io if labels.nil?

        io << "<div class='usage-axis' aria-hidden='true'>"
        labels.each { |i, text| io << "<span style='left: #{(100.0 * i / columns.size).round(2)}%'>#{h(text)}</span>" }
        io << '</div>'
    end

    # The columns of a chart over the periods: value and tip from the
    # chart (CHARTS), a link to the day in five-minute steps for the days
    # whose looks are still kept, weekends marked.
    def self.columns(periods, today, value, tip)
        weekly = periods.any? { |p| p[:from] != p[:to] }
        periods.map do |p|
            linked = !weekly && p[:counted] && p[:from] > today - SAMPLE_DAYS
            { :value => value.(p), :tip => "#{UsageStats.period_label(p)}: #{tip.(p)}",
              :href => linked ? "/statistik?server=#{p[:from].iso8601}" : nil,
              :weekend => !weekly && (p[:from].saturday? || p[:from].sunday?) }
        end
    end

    def self.cpu_tip(p)
        return 'nicht gemessen' if p[:cpu_mean].nil?

        "im Mittel #{decimal(p[:cpu_mean])} %, in der vollsten Stunde #{decimal(p[:cpu_hour])} %, höchstens #{decimal(p[:cpu_max])} % über fünf Minuten"
    end

    def self.pair_tip(pair) = pair ? "#{gb(pair[0])} von #{gb(pair[1])} (#{decimal(share(pair))} %)" : 'nicht gemessen'

    CHARTS = [
        ['Prozessoren', 'in der vollsten Stunde, im Mittel über alle Prozessoren', ->(p) { p[:cpu_hour] }, ->(p) { cpu_tip(p) }],
        ['Arbeitsspeicher', 'der höchste Stand', ->(p) { share(p[:ram]) }, ->(p) { pair_tip(p[:ram]) }],
        ['Festplatte des Systems', 'am Ende des Tages', ->(p) { share(p[:root]) }, ->(p) { pair_tip(p[:root]) }],
        ['Festplatte der Workspaces', '/user, am Ende des Tages', ->(p) { share(p[:user]) }, ->(p) { pair_tip(p[:user]) }],
    ].freeze

    # The section of /statistik. since: the first measured hour (nil:
    # none yet); today: the day to count the SAMPLE_DAYS back from. The
    # processors and the memory stand in the course above (UsageStats.
    # course_html), beside the students; here the two disks.
    def self.html(periods, since:, today:)
        io = +"<div class='usage-charts'><h2 class='mt-4' id='server'>Server</h2>"
        if since.nil?
            io << '<p>Noch keine Zahlen. Der Workspace liest alle fünf Minuten, wie ausgelastet der Server ist.</p></div>'
            return io
        end
        weekly = periods.any? { |p| p[:from] != p[:to] }
        io << "<p>Prozessoren und Arbeitsspeicher stehen oben im Verlauf, neben den Lernenden; hier die beiden Festplatten, " \
              "#{weekly ? 'am Ende jeder Woche' : 'am Ende jedes Tages'}. Gemessen wird alle fünf Minuten, seit #{Time.at(since).strftime('%-d.%-m.%Y')}. " \
              "Je Stunde bleiben Mittel und Höchstwerte ohne Ende, die einzelnen Messungen #{SAMPLE_DAYS} Tage" \
              "#{weekly ? '' : '; ein Klick auf einen dieser Tage zeigt ihn in Fünf-Minuten-Schritten'}.</p>"
        labels = UsageStats.axis_labels(periods)
        io << "<div class='server-plots'>"
        CHARTS.drop(2).each do |title, note, value, tip|
            io << "<section><h3 class='h6 mb-1'>#{h(title)}</h3><p class='server-note'>#{h(note)}</p>#{plot(columns(periods, today, value, tip), labels)}</section>"
        end
        io << '</div>'
        pct = ->(v) { v.nil? ? '–' : "#{decimal(v)} %" }
        io << "<details class='usage-table-view'><summary>Als Tabelle</summary><div class='table-responsive'><table class='table table-sm usage-table'>" \
              "<thead><tr><th>#{weekly ? 'Woche' : 'Tag'}</th><th class='text-end'>System</th><th class='text-end'>Workspaces</th></tr></thead><tbody>"
        periods.each do |p|
            io << "<tr><td>#{UsageStats.period_label(p)}</td><td class='text-end'>#{pct.(share(p[:root]))}</td><td class='text-end'>#{pct.(share(p[:user]))}</td></tr>"
        end
        io << '</tbody></table></div></details></div>'
    end

    # Fixed steps of 20 %, so that the colours mean the same in every window
    def self.rhythm_html(rhythm)
        io = +"<div class='usage-rhythm' role='group' aria-label='Prozessoren nach Wochentag und Uhrzeit'><span></span>"
        24.times { |hour| io << "<span class='usage-hour'>#{hour % 3 == 0 ? hour : ''}</span>" }
        rhythm.each_with_index do |row, wday|
            io << "<span class='usage-day'>#{UsageStats::WEEKDAYS[wday]}</span>"
            row.each_with_index do |value, hour|
                when_ = "#{UsageStats::WEEKDAYS[wday]} #{hour} bis #{hour + 1} Uhr"
                if value.nil?
                    io << "<span class='usage-cell' data-tip='#{when_}: nicht gemessen'></span>"
                else
                    io << "<span class='usage-cell usage-level-#{(value / 20).ceil.clamp(1, 5)}' data-tip='#{when_}: #{decimal(value)} %'></span>"
                end
            end
        end
        io << '</div>'
        io << "<p class='usage-legend'>0 % #{(1..5).map { |l| "<i class='usage-swatch usage-level-#{l}'></i>" }.join} 100 %, in Schritten von 20 %</p>"
    end

    # One day in five-minute steps: samples [{ ts:, cpu:, ram_used:,
    # ram_total:, root_used:, root_total:, user_used:, user_total: }] of
    # that day, oldest first.
    def self.day_html(day, samples, today:)
        start = Time.new(day.year, day.month, day.day).to_i
        slot = ->(ts) { ((ts - start) / 300).clamp(0, 287) }
        by_slot = samples.group_by { |s| slot.(s[:ts]) }.transform_values(&:last)
        time = ->(i) { format('%d:%02d', i / 12, i % 12 * 5) }
        labels = (0..21).step(3).map { |hour| [hour * 12, hour.to_s] }
        prev_day = day - 1
        next_day = day + 1
        io = +"<div class='usage-charts'>"
        io << "<nav class='server-day-nav mt-4' aria-label='Tage'>"
        io << "<a class='btn btn-sm btn-outline-primary' href='/statistik#server'><i class='bi bi-arrow-left'></i>Statistik</a>"
        io << (prev_day > today - SAMPLE_DAYS ? "<a class='btn btn-sm btn-outline-secondary' href='/statistik?server=#{prev_day.iso8601}'><i class='bi bi-chevron-left'></i>#{UsageStats.date(prev_day)}</a>" : '')
        io << (next_day <= today ? "<a class='btn btn-sm btn-outline-secondary' href='/statistik?server=#{next_day.iso8601}'>#{UsageStats.date(next_day)}<i class='bi bi-chevron-right ms-2 me-0'></i></a>" : '')
        io << '</nav>'
        io << "<h1 class='mt-4'>Server am #{UsageStats::WEEKDAYS[day.cwday - 1]} #{day.strftime('%-d.%-m.%Y')}</h1>"
        if samples.empty?
            io << '<p>An diesem Tag gibt es keine Messungen.</p></div>'
            return io
        end
        io << '<p>Je Säule fünf Minuten, darunter die Stunden des Tages.</p>'
        charts = [
            ['Prozessoren', 'im Mittel über alle Prozessoren seit der Messung davor', ->(s) { s[:cpu] }, ->(s) { s[:cpu].nil? ? 'nicht gemessen' : "#{decimal(s[:cpu])} %" }],
            ['Arbeitsspeicher', 'in Gebrauch', ->(s) { share([s[:ram_used], s[:ram_total]]) }, ->(s) { pair_tip([s[:ram_used], s[:ram_total]]) }],
        ]
        io << "<div class='server-plots'>"
        charts.each do |title, note, value, tip|
            columns = (0...288).map do |i|
                s = by_slot[i]
                { :value => s && value.(s), :tip => "#{time.(i)} Uhr: #{s ? tip.(s) : 'nicht gemessen'}" }
            end
            io << "<section><h3 class='h6 mb-1'>#{h(title)}</h3><p class='server-note'>#{h(note)}</p>#{plot(columns, labels, :fine => true)}</section>"
        end
        io << '</div>'
        last = samples.reverse.find { |s| s[:root_total] || s[:user_total] }
        if last
            io << "<p class='mt-3'>Festplatten um #{Time.at(last[:ts]).strftime('%-H:%M')} Uhr: System #{pair_tip([last[:root_used], last[:root_total]]) if last[:root_total]}, "                   "Workspaces #{pair_tip([last[:user_used], last[:user_total]]) if last[:user_total]}.</p>"
        end
        io << '</div>'
    end
end
