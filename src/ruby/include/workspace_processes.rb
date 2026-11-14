# What keeps a Workspace busy, for the administration (»Laufende
# Workspaces« on /admin, the button »Prozesse«): its processes with their
# share of a core right now, memory and how long they run. Read inside the
# container from /proc twice, half a second apart (SCRIPT), so the share is
# the present one, not the average of a process's life that ps shows.
# Each process is named by its program and the file it runs (»python3
# schleife.py«), without further arguments (the maintainer's decision,
# 14 November 2026: enough to see a runaway loop, nothing more of the
# student's work); the processes of VS Code by what they are. Plain Ruby,
# tested in workspace_processes_test.rb.
module WorkspaceProcesses
    # one line per process: »P pid ticks starttime rss comm \x1f cmdline«
    # (the arguments of cmdline separated by \x1f), a marker between the
    # two readings, and before them the clock rate, the page size and the
    # uptime
    SCRIPT = <<~'SH'
        printf 'H %s %s %s\n' "$(getconf CLK_TCK 2>/dev/null || echo 100)" "$(getconf PAGESIZE 2>/dev/null || echo 4096)" "$(cut -d' ' -f1 /proc/uptime)"
        snap() {
            for p in /proc/[0-9]*; do
                stat=$(cat "$p/stat" 2>/dev/null) || continue
                rest=${stat##*) }
                set -- $rest
                comm=$(cat "$p/comm" 2>/dev/null | tr '\t\n' '  ')
                cmd=$(tr '\000\t\n' '\037  ' < "$p/cmdline" 2>/dev/null)
                printf 'P %s %s %s %s %s\t%s\n' "${p#/proc/}" "$(( ${12} + ${13} ))" "${20}" "${22}" "$comm" "$cmd"
            done
        }
        snap
        echo M
        sleep 0.5
        snap
    SH
    INTERVAL = 0.5

    # Ends one process (»Beenden« in the list): only when the number still
    # names the process that was listed (its start), TERM first, KILL after
    # two seconds. Prints ended, gone (no longer there) or changed (another
    # process has the number now). $1 the pid, $2 its start.
    END_SCRIPT = <<~'SH'
        start_of() { s=$(cat "/proc/$1/stat" 2>/dev/null) || return 1; r=${s##*) }; set -- $r; echo "${20}"; }
        now=$(start_of "$1") || { echo gone; exit 0; }
        [ "$now" = "$2" ] || { echo changed; exit 0; }
        kill -TERM "$1" 2>/dev/null
        i=0
        while [ $i -lt 20 ] && [ "$(start_of "$1")" = "$2" ]; do sleep 0.1; i=$((i + 1)); done
        [ "$(start_of "$1")" = "$2" ] && kill -KILL "$1" 2>/dev/null
        echo ended
    SH

    # [{ :pid, :label, :cpu (percent of a core), :rss (bytes), :seconds }],
    # the busiest first; processes that ended between the readings drop out
    def self.parse(output)
        hz = 100
        page = 4096
        uptime = 0.0
        first = {}
        second = {}
        target = first
        output.to_s.each_line do |line|
            line = line.chomp
            if line.start_with?('H ')
                _, a, b, c = line.split(' ')
                hz = a.to_i.positive? ? a.to_i : 100
                page = b.to_i.positive? ? b.to_i : 4096
                uptime = c.to_f
            elsif line == 'M'
                target = second
            elsif line.start_with?('P ')
                head, cmd = line.split("\t", 2)
                _, pid, ticks, start, rss, comm = head.split(' ', 6)
                target[pid.to_i] = { :ticks => ticks.to_i, :start => start.to_i, :rss => rss.to_i, :comm => comm.to_s.strip,
                                     :argv => cmd.to_s.split("\x1f").reject(&:empty?) }
            end
        end
        second.filter_map do |pid, b|
            a = first[pid]
            next unless a && a[:start] == b[:start]
            # the reading itself, and the Workspace's own supervision (s6
            # and the run scripts of its services), which keeps nothing busy
            next if b[:argv].join(' ').include?('getconf CLK_TCK')
            next if b[:comm].start_with?('s6-') || b[:argv].first.to_s.include?('s6-') ||
                    (%w[bash sh].include?(File.basename(b[:argv].first.to_s)) && b[:argv][1].to_s.match?(%r{(\A|/)run\z}))

            {
                :pid => pid,
                # when it started (clock ticks since boot): ending it checks
                # that the number still names the same process
                :start => b[:start],
                :label => label(b[:argv], b[:comm]),
                :cpu => ((b[:ticks] - a[:ticks]).to_f / hz / INTERVAL * 100).round(1),
                :rss => b[:rss] * page,
                :seconds => [(uptime - b[:start].to_f / hz).round, 0].max,
            }
        end.sort_by { |p| [-p[:cpu], -p[:rss]] }
    end

    # A process as the administration reads it: the program and the file it
    # runs (inside /workspace relative to it, elsewhere its name), or what
    # part of VS Code it is (an extension's process by the extension)
    def self.label(argv, comm)
        joined = argv.join(' ')
        if joined.include?('code-server') || joined.include?('/out/server-main') || joined.include?('bootstrap-fork')
            return 'VS Code: Erweiterungen' if joined.include?('extensionHost')
            return 'VS Code: Dateien beobachten' if joined.include?('watcherService') || joined.include?('fileWatcher')
            return 'VS Code: Terminal' if joined.include?('ptyHost')

            return 'VS Code'
        end
        return comm if argv.empty?

        program = File.basename(argv.first)
        # a shell running a command line: its words are not a file
        return "#{program} -c" if argv[1] == '-c'

        file = argv.drop(1).find { |a| !a.start_with?('-') && (a.include?('.') || a.include?('/')) }
        return program unless file

        # a file of a VS Code extension (its language server): the
        # extension's name without version, not its long path
        extension = file.match(%r{(?:\A|/)\.?(?:vscode-server/)?extensions/([^/]+)/})
        return "VS Code: Erweiterung #{extension[1].sub(/-\d+(?:\.\d+)+.*\z/, '')}" if extension

        shown = file.start_with?('/workspace/') ? file.delete_prefix('/workspace/') : File.basename(file)
        "#{program} #{shown}"
    end
end
