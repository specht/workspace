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
        name=$(git config user.name 2>/dev/null | head -n 1)
        email=$(git config user.email 2>/dev/null | head -n 1)
        [ -n "$name" ] && echo "NAME $name"
        [ -n "$email" ] && echo "EMAIL $email"
        git rev-parse --is-inside-work-tree >/dev/null 2>&1 || { echo "NOGIT"; exit 0; }
        echo "TOP $(git rev-parse --show-toplevel 2>/dev/null)"
        echo "COUNT $(git rev-list --count HEAD 2>/dev/null || echo 0)"
        if [ -n "$email" ]; then
            echo "MINE $(git log --format=%ae 2>/dev/null | grep -cxF -- "$email")"
        fi
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
            when 'NOGIT' then return { :repo => true, :git => false }.merge(state.slice(:name, :email))
            when /\ANAME (.+)\z/ then state[:name] = $1.strip
            when /\ATOP (.*)\z/ then state[:top] = $1
            when /\ACOUNT (\d+)\z/ then state[:commits] = $1.to_i
            when /\AEMAIL (.+)\z/ then state[:email] = $1.strip
            when /\AMINE (\d+)\z/ then state[:mine] = $1.to_i
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
        return identity_line(state) if repo.to_s.empty?
        return "#{code(repo)} gibt es nicht." unless state[:repo]
        return "#{code(repo)} ist kein Git-Repository." unless state[:git]

        lines = []
        lines << "Repository #{code(repo)}, Branch #{code(state[:branch] || '?')}" +
            (state[:upstream] ? " → #{code(state[:upstream])}" : ' (ohne Remote-Branch)')
        mine = state[:mine] ? " (#{state[:mine]} von #{code(state[:email])})" : ''
        lines << "#{plural(state[:commits], 'Commit', 'Commits')}#{mine}, Remotes: #{state[:remotes].empty? ? 'keine' : state[:remotes].join(', ')}"
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

    # git config user.name and user.email, as Git sees them in /workspace.
    def self.identity_line(state)
        name = state[:name] ? code(state[:name]) : 'nicht gesetzt'
        email = state[:email] ? code(state[:email]) : 'nicht gesetzt'
        "`git config user.name`: #{name}\n\n`git config user.email`: #{email}"
    end

    # The example values from the Handbuch, copied unchanged.
    PLACEHOLDER_NAMES = ['Vorname Nachname'].freeze
    PLACEHOLDER_EMAILS = ['deine.mail@example.org'].freeze
    SET_IDENTITY = "```\ngit config --global user.name \"Vorname Nachname\"\ngit config --global user.email \"deine.mail@example.org\"\n```"

    # [ok, problem, message] for expect identity: Git knows a name and an
    # e-mail address, and they are not the examples from the Handbuch.
    def self.evaluate_identity(state)
        missing = []
        missing << 'deinen Namen' unless state[:name]
        missing << 'deine E-Mail-Adresse' unless state[:email]
        unless missing.empty?
            return [false, :no_identity, "Git kennt #{missing.join(' und ')} noch nicht. Ohne beides kannst du nicht committen. Im Terminal, mit deinen eigenen Angaben:\n\n#{SET_IDENTITY}"]
        end
        copied = []
        copied << "den Namen #{code(state[:name])}" if PLACEHOLDER_NAMES.include?(state[:name])
        copied << "die E-Mail-Adresse #{code(state[:email])}" if PLACEHOLDER_EMAILS.include?(state[:email])
        unless copied.empty?
            return [false, :placeholder, "Git kennt #{copied.join(' und ')} aus dem Beispiel. Setze deine eigenen Angaben ein und führe den Befehl noch einmal aus."]
        end
        [true, nil, nil]
    end

    # What to git add: the one folder all changes are in (»lena/«), else -A.
    def self.add_target(paths)
        tops = paths.map { |p| p.include?('/') ? p.split('/').first : nil }.uniq
        tops.size == 1 && tops.first ? "#{tops.first}/" : '-A'
    end

    # Returns [ok, problem, message] for the expectations of a git check.
    # root is where repo lies (the Workspace folder as the container sees it).
    def self.evaluate(repo, expect, state, root: '/workspace')
        # Without a repo, only who Git thinks you are (git config --global).
        return evaluate_identity(state) if repo.to_s.empty?

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
        if expect['identity']
            result = evaluate_identity(state)
            return result unless result.first
        end
        # Without a name and e-mail Git refuses to commit, so this comes first.
        if expect['min_commits'] && state[:email].nil?
            return [false, :no_identity, "Git weiß noch nicht, wer du bist. Ohne Namen und E-Mail-Adresse kannst du nicht committen. Im Terminal:\n\n#{SET_IDENTITY}"]
        end
        if expect['committed'] && !state[:changes].empty?
            paths = state[:changes].map { |_, path| path }
            shown = paths.first(3).map { |p| code(p) }.join(', ')
            more = paths.size > 3 ? " und #{paths.size - 3} weitere" : ''
            untracked = state[:changes].all? { |xy, _| xy == '??' }
            what = untracked ? 'neu und noch nie committet' : 'geändert, aber nicht committet'
            steps = ["cd /workspace/#{repo}"]
            steps << 'git pull' if expect['pushed'] && state[:upstream]
            steps << "git add #{add_target(paths)}"
            steps << 'git commit -m "Was du gemacht hast"'
            steps << 'git push' if expect['pushed']
            return [false, :uncommitted, "#{plural(paths.size, 'Datei ist', 'Dateien sind')} #{what}: #{shown}#{more}.\n\nIm Terminal:\n\n```\n#{steps.join("\n")}\n```\n\nSchreib in die Commit-Nachricht in einem Satz, was du gemacht hast."]
        end
        if expect['min_commits']
            if state[:mine].to_i.zero?
                return [false, :too_few_commits, "Von dir (#{code(state[:email])}) gibt es in #{code(repo)} noch keinen Commit. Lege deine Dateien an, dann `git add` und `git commit`."]
            end
            if state[:mine].to_i < expect['min_commits']
                return [false, :too_few_commits, "In #{code(repo)} gibt es #{plural(state[:mine].to_i, 'Commit', 'Commits')} von #{code(state[:email])}, verlangt #{expect['min_commits'] == 1 ? 'ist einer' : "sind #{expect['min_commits']}"}."]
            end
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
            if state[:ahead].to_i > 0 && state[:behind].to_i > 0
                return [false, :not_pushed, "Dein Stand und der auf dem Server sind auseinandergelaufen: #{plural(state[:ahead], 'Commit', 'Commits')} von dir fehlen dort, #{plural(state[:behind], 'Commit', 'Commits')} von anderen fehlen bei dir. Im Terminal:\n\n```\ncd /workspace/#{repo}\ngit pull\ngit push\n```\n\nLehnt Git das ab, lösche nichts und frag nach."]
            end
            if state[:ahead].to_i > 0
                return [false, :not_pushed, "#{plural(state[:ahead], 'Commit ist', 'Commits sind')} noch nicht gepusht. Im Terminal:\n\n```\ncd /workspace/#{repo}\ngit push\n```"]
            end
        end
        [true, nil, nil]
    end
end
