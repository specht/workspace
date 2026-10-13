require 'shellwords'

# Check type git (docs/aufgaben.md): is the work in a repository committed
# and pushed? Made for the sandboxes, where a student may work for weeks
# without ever pushing.
#
# The server never runs Git on a student's repository: settings in
# .git/config can make Git run commands. STATUS_SCRIPT runs inside the
# student's own container, as the student, and only reads:
# GIT_OPTIONAL_LOCKS=0 keeps git status from writing the index, and nothing
# talks to the network, so »pushed« means »pushed as far as the repository
# knows«.
module GitCheck
    STATUS_SCRIPT = <<~'SH'
        cd "$1" 2>/dev/null || { echo "NOREPO"; exit 0; }
        export GIT_OPTIONAL_LOCKS=0 GIT_TERMINAL_PROMPT=0
        git rev-parse --is-inside-work-tree >/dev/null 2>&1 || { echo "NOGIT"; exit 0; }
        echo "TOP $(git rev-parse --show-toplevel 2>/dev/null)"
        echo "COUNT $(git rev-list --count HEAD 2>/dev/null || echo 0)"
        git remote 2>/dev/null | sed 's/^/REMOTE /'
        git -c core.fsmonitor=false -c core.untrackedCache=false status --porcelain=v1 --branch -uall 2>/dev/null | head -n 300 | sed 's/^/STATUS /'
    SH

    # The command line for docker exec (arguments, not a shell string).
    def self.exec_args(fs_tag, repo, uid: '1000:1000')
        ['docker', 'exec', '-u', uid, "hs_code_#{fs_tag}", 'sh', '-c', STATUS_SCRIPT, 'sh', "/workspace/#{repo}"]
    end

    # Reads the output of STATUS_SCRIPT.
    def self.parse(output)
        state = { :repo => true, :git => true, :remotes => [], :commits => 0, :changes => [] }
        output.to_s.each_line do |line|
            line = line.chomp
            case line
            when 'NOREPO' then return { :repo => false, :git => false }
            when 'NOGIT' then return { :repo => true, :git => false }
            when /\ATOP (.*)\z/ then state[:top] = $1
            when /\ACOUNT (\d+)\z/ then state[:commits] = $1.to_i
            when /\AREMOTE (.+)\z/ then state[:remotes] << $1.strip
            when /\ASTATUS ## (.*)\z/ then state.merge!(parse_branch($1))
            when /\ASTATUS (..) (.*)\z/ then state[:changes] << [$1, $2]
            end
        end
        state
    end

    def self.parse_branch(text)
        result = { :branch_line => text }
        if text =~ /\ANo commits yet on (\S+)/ || text =~ /\AInitial commit on (\S+)/
            result[:branch] = $1
            return result
        end
        if text.start_with?('HEAD (no branch)')
            result[:detached] = true
            return result
        end
        m = text.match(/\A(\S+?)(?:\.\.\.(\S+))?(?: \[(.*)\])?\z/)
        return result unless m

        result[:branch] = m[1]
        result[:upstream] = m[2]
        (m[3] || '').split(',').map(&:strip).each do |part|
            result[:ahead] = $1.to_i if part =~ /\Aahead (\d+)\z/
            result[:behind] = $1.to_i if part =~ /\Abehind (\d+)\z/
            result[:upstream_gone] = true if part == 'gone'
        end
        result
    end

    def self.code(text)
        "`#{text.to_s.gsub('`', "'")}`"
    end

    def self.plural(n, one, many)
        n == 1 ? "1 #{one}" : "#{n} #{many}"
    end

    # What was read, for the student.
    def self.describe(repo, state)
        return "#{code(repo)} gibt es nicht." unless state[:repo]
        return "#{code(repo)} ist kein Git-Repository." unless state[:git]

        lines = []
        lines << "Repository #{code(repo)}, Branch #{code(state[:branch] || '?')}" +
            (state[:upstream] ? " → #{code(state[:upstream])}" : ' (ohne Remote-Branch)')
        lines << "#{plural(state[:commits], 'Commit', 'Commits')}, Remotes: #{state[:remotes].empty? ? 'keine' : state[:remotes].join(', ')}"
        if state[:ahead].to_i > 0 || state[:behind].to_i > 0
            lines << "#{state[:ahead].to_i} noch nicht gepusht, #{state[:behind].to_i} noch nicht geholt"
        end
        if state[:changes].empty?
            lines << 'Keine Änderungen seit dem letzten Commit.'
        else
            shown = state[:changes].first(10).map { |xy, path| "#{xy} #{path}" }
            more = state[:changes].size > 10 ? "\n… und #{state[:changes].size - 10} weitere" : ''
            lines << "`git status` meldet:\n\n```\n#{shown.join("\n")}#{more}\n```"
        end
        lines.join("\n\n")
    end

    # Returns [ok, problem, message] for the expectations of a git check.
    # root is where repo lies (the Workspace folder as the container sees it).
    def self.evaluate(repo, expect, state, root: '/workspace')
        unless state[:repo]
            return [false, :no_repo, "#{code(repo)} gibt es in deinem Workspace nicht. Hast du das Repository geklont? Es gehört nach `/workspace/#{repo}`."]
        end
        unless state[:git]
            return [false, :not_git, "#{code(repo)} ist kein Git-Repository. Hast du den Ordner selbst angelegt, statt ihn zu klonen?"]
        end
        if state[:top] && root && state[:top] != File.join(root, repo)
            outer = state[:top].delete_prefix("#{root}/")
            return [false, :not_git, "#{code(repo)} ist kein eigenes Git-Repository, sondern ein Ordner im Repository #{code(outer)}. Hast du das richtige Repository hierher geklont?"]
        end
        if expect['has_remote'] && state[:remotes].empty?
            return [false, :no_remote, "Das Repository #{code(repo)} kennt kein Remote, also keinen Ort, an den es pushen kann. `git remote -v` zeigt nichts."]
        end
        if expect['min_commits'] && state[:commits] < expect['min_commits']
            return [false, :too_few_commits, "In #{code(repo)} gibt es #{plural(state[:commits], 'Commit', 'Commits')}, verlangt #{expect['min_commits'] == 1 ? 'ist einer' : "sind #{expect['min_commits']}"}."]
        end
        if expect['committed'] && !state[:changes].empty?
            paths = state[:changes].map { |_, path| path }
            shown = paths.first(3).map { |p| code(p) }.join(', ')
            more = paths.size > 3 ? " und #{paths.size - 3} weitere" : ''
            untracked = state[:changes].all? { |xy, _| xy == '??' }
            what = untracked ? 'neu und noch nie committet' : 'geändert, aber nicht committet'
            push = expect['pushed'] ? ' Danach `git push`.' : ''
            return [false, :uncommitted, "#{plural(paths.size, 'Datei ist', 'Dateien sind')} #{what}: #{shown}#{more}. Im Terminal, im Ordner #{code(repo)}: `git add -A` und `git commit -m \"…\"` mit einem Satz, was du gemacht hast.#{push}"]
        end
        if expect['pushed']
            if state[:detached]
                return [false, :detached, "In #{code(repo)} ist gerade kein Branch ausgewählt (»detached HEAD«). Wechsle mit `git switch main` zurück, bevor du weiterarbeitest."]
            end
            if state[:upstream].nil? || state[:upstream_gone]
                branch = state[:branch] || 'main'
                remote = state[:remotes].first || 'origin'
                return [false, :no_upstream, "Der Branch #{code(branch)} ist noch mit keinem Branch auf dem Server verbunden. Im Terminal: `git push -u #{remote} #{branch}`. Danach reicht `git push`."]
            end
            if state[:ahead].to_i > 0
                return [false, :not_pushed, "#{plural(state[:ahead], 'Commit ist', 'Commits sind')} noch nicht gepusht. Im Terminal, im Ordner #{code(repo)}: `git push`."]
            end
        end
        [true, nil, nil]
    end
end
