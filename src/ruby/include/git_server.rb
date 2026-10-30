require 'base64'
require 'digest'
require 'fileutils'
require 'open3'
require 'securerandom'
require 'tmpdir'

# Git repositories for groups, kept by the Workspace itself
# (docs/plan-git-gruppen.md, docs/teacher-access.md »Repositorys im
# Workspace«). Off unless GIT_SERVER = true in credentials.rb.
#
# A small SSH server (docker/git-server) holds the repositories in
# data/git-repos/<git id>/<name>.git, one git id per group (GroupSetting.git_id),
# so that renaming a group changes no address. Workspaces reach it as
# git@git.workspace on port 22 and nothing else (the peer firewall's label of
# the case server). Everything runs there as the user git; hooks and config
# are out of reach of students.
#
# The backend tells the server who may do what through two files it writes
# into data/git-server/config, which the server reads:
#
#   authorized_keys   one line per key: command="/usr/local/bin/git-gate ACCOUNT",restrict KEY
#   access            one line per account and repository folder: ACCOUNT GIT_ID
#
# ACCOUNT is a hash of the e-mail address (the Workspace's fs_tag), so the
# server's files hold no addresses. The gate (docker/git-server/git-gate)
# lets an account run git-upload-pack, git-receive-pack or
# git-upload-archive on a repository in a folder it has a line for.
#
# A group's folder holds the repository of an activity (a class repository,
# created on the class page) and free ones a teacher creates on /repos. Every
# person has a folder of their own too (User.git_id), for repositories to
# practise with: only that person may clone, push and read them, not their
# teachers and not the administration, who sees only that they exist and may
# delete them (docs/plan-git-gruppen.md: practice is private; shared work
# belongs in the class repositories).
#
# How students get a key is the teacher's choice per group
# (GroupSetting.git_keys): KEYS_HANDED_IN, they create a key pair and hand in
# the public key on the page; KEYS_AUTO, the site writes a key pair of its
# own into each student's Workspace (~/.ssh/workspace_git) with a block in
# ~/.ssh/config and the server's key in ~/.ssh/known_hosts.
#
# Plain Ruby around git and ssh-keygen, tested on its own
# (test/git_server_test.rb).
module GitServer
    HOST = 'git.workspace'
    USER = 'git'
    # The server's user, who owns the repositories (docker/git-server).
    UID = 1000
    # In the backend's container.
    REPOS = '/git-repos'
    CONFIG = '/git-server/config'
    HOST_KEY = '/git-server/host/ssh_host_ed25519_key.pub'
    KEYS_HANDED_IN = 'handed_in'
    KEYS_AUTO = 'auto'
    KEY_MODES = [KEYS_HANDED_IN, KEYS_AUTO].freeze
    # The key pair the site writes in KEYS_AUTO, in the student's ~/.ssh.
    AUTO_KEY = 'workspace_git'
    # Marks the block the site manages in ~/.ssh/config and ~/.ssh/known_hosts.
    BEGIN_MARK = '# >>> Workspace-Git (von der Website eingetragen)'
    END_MARK = '# <<< Workspace-Git'
    KEY_TYPES = %w[ssh-ed25519 ecdsa-sha2-nistp256 ecdsa-sha2-nistp384 ecdsa-sha2-nistp521 ssh-rsa
                   sk-ssh-ed25519@openssh.com sk-ecdsa-sha2-nistp256@openssh.com].freeze
    GIT_ID = /\A[a-z0-9]{10}\z/
    NAME = /\A[a-z0-9][a-z0-9-]{0,62}\z/
    ACCOUNT = /\A[a-z0-9]{1,32}\z/
    # Settings of every repository: no force push, no deleting a branch, every
    # object checked, no push larger than this.
    REPO_CONFIG = {
        'receive.denyNonFastForwards' => 'true',
        'receive.denyDeletes' => 'true',
        'receive.fsckObjects' => 'true',
        'receive.maxInputSize' => '52428800',
        'core.logAllRefUpdates' => 'true',
    }.freeze
    STARTER_AUTHOR = ['Workspace', 'workspace@git.workspace'].freeze
    # A person's own repositories, at most this many.
    PERSONAL_MAX = 10
    DESCRIPTION_MAX = 200
    # What git init writes into description: no description.
    NO_DESCRIPTION = /\AUnnamed repository/

    def self.enabled?
        defined?(GIT_SERVER) && GIT_SERVER == true
    end

    def self.new_git_id
        SecureRandom.alphanumeric(10).downcase
    end

    def self.address(git_id, name)
        "#{USER}@#{HOST}:#{git_id}/#{name}.git"
    end

    # [git_id, name] of an address on this server, or nil.
    def self.parse_address(address)
        m = %r{\A#{USER}@#{Regexp.escape(HOST)}:([a-z0-9]{10})/([a-z0-9][a-z0-9-]{0,62})\.git\z}.match(address.to_s.strip)
        m && [m[1], m[2]]
    end

    def self.local?(address)
        !parse_address(address).nil?
    end

    # --- keys -----------------------------------------------------------------

    # A public key as { type:, blob:, line: } with the comment left out (it
    # often holds a name), or nil if it is none.
    def self.parse_key(text)
        fields = text.to_s.strip.split(/\s+/)
        return nil unless fields.size >= 2 && KEY_TYPES.include?(fields[0])

        blob = Base64.strict_decode64(fields[1])
        # The blob starts with its own type, length-prefixed.
        length = blob[0, 4].unpack1('N')
        return nil unless length && blob[4, length] == fields[0]

        { :type => fields[0], :blob => blob, :line => "#{fields[0]} #{fields[1]}" }
    rescue ArgumentError
        nil
    end

    # As ssh-keygen -l shows it: SHA256:… without padding.
    def self.fingerprint(key_line)
        key = parse_key(key_line)
        key && "SHA256:#{Base64.strict_encode64(Digest::SHA256.digest(key[:blob])).delete('=')}"
    end

    # authorized_keys for sshd: entries { account => key line or lines } (a
    # key handed in and the automatic one).
    def self.authorized_keys(entries)
        entries.sort.flat_map do |account, lines|
            next [] unless account.to_s.match?(ACCOUNT)

            Array(lines).filter_map { |line| parse_key(line) }.map { |key| key[:line] }.uniq.map do |line|
                %(command="/usr/local/bin/git-gate #{account}",restrict #{line})
            end
        end.join("\n") + "\n"
    end

    # Who may use which folder, for access: the people of each group's folder
    # (group_people: { git id => [accounts] }) and each person's own folder
    # (personal: { git id => account }), which no one else gets, not a
    # teacher of the person's group either. { account => [git ids] }
    def self.access_map(group_people:, personal:)
        map = Hash.new { |hash, key| hash[key] = [] }
        group_people.each { |id, accounts| accounts.each { |account| map[account] << id } }
        personal.each { |id, account| map[account] << id }
        map
    end

    # May a person read a folder's repositories on the site? A group's folder:
    # its members and teachers and the administration (group_reader); a
    # person's own folder: that person alone (owner: whose it is).
    def self.may_read?(owner:, group_reader:, email:)
        owner ? owner == email : group_reader
    end

    # Why name cannot be a new repository in a folder, or nil. taken: the
    # names there; reserved: names of activities with a class repository
    # (their repository is made on the class page); limit: at most this many.
    def self.name_problem(name, taken: [], reserved: [], limit: nil)
        return "Du hast schon #{limit} Repositorys; lösche eins, bevor du ein neues anlegst." if limit && taken.size >= limit
        return 'Gib dem Repository einen Namen.' if name.to_s.empty?
        unless name.match?(NAME)
            return 'Nur Kleinbuchstaben, Ziffern und Bindestriche, vorne ein Buchstabe oder eine Ziffer, höchstens 63 Zeichen.'
        end
        return 'Ein Repository mit diesem Namen gibt es hier schon.' if taken.include?(name)
        if reserved.include?(name)
            return 'So heißt eine Aktivität; ihr Repository legst du auf der Klassenseite an. Nimm einen anderen Namen.'
        end

        nil
    end

    # A description as one line, without control characters, cut at DESCRIPTION_MAX.
    def self.clean_description(text)
        text.to_s.gsub(/[[:cntrl:]]+/, ' ').squeeze(' ').strip[0, DESCRIPTION_MAX].to_s.strip
    end

    # access for the gate: { account => [git ids] }.
    def self.access(map)
        map.sort.flat_map do |account, ids|
            next [] unless account.to_s.match?(ACCOUNT)

            ids.select { |id| id.to_s.match?(GIT_ID) }.uniq.sort.map { |id| "#{account} #{id}" }
        end.join("\n") + "\n"
    end

    # Writes a file of the config folder at once, so the server never reads
    # half of it.
    def self.write_config(name, text, dir: CONFIG)
        FileUtils.mkdir_p(dir)
        # sshd ignores authorized_keys in a folder others may write to (StrictModes).
        File.chmod(0o755, dir)
        path = File.join(dir, name)
        temp = "#{path}.#{SecureRandom.hex(4)}"
        File.write(temp, text)
        File.chmod(0o644, temp)
        File.rename(temp, path)
    end

    # The line for known_hosts, from the server's public host key.
    def self.known_hosts_line(host_key_text)
        key = parse_key(host_key_text)
        key && "#{HOST} #{key[:line]}"
    end

    # text with the block between BEGIN_MARK and END_MARK replaced by lines
    # (or added at the end); nil lines removes the block.
    def self.replace_block(text, lines)
        kept = []
        inside = false
        text.to_s.each_line(chomp: true) do |line|
            if line == BEGIN_MARK
                inside = true
            elsif line == END_MARK && inside
                inside = false
            elsif !inside
                kept << line
            end
        end
        kept.pop while kept.last == ''
        kept += ['', BEGIN_MARK, *lines, END_MARK] if lines
        kept.shift while kept.first == ''
        kept.empty? ? '' : kept.join("\n") + "\n"
    end

    def self.ssh_config_lines
        ["Host #{HOST}", "    User #{USER}", "    IdentityFile ~/.ssh/#{AUTO_KEY}", '    IdentitiesOnly yes']
    end

    # KEYS_AUTO: the key pair, the block in ~/.ssh/config and the host key
    # in ~/.ssh/known_hosts, in a Workspace's folder (home, as the backend
    # sees it). An existing key pair stays. Returns the public key's line.
    def self.install_auto_key(home, host_key_text:, uid: UID)
        ssh = File.join(home, '.ssh')
        FileUtils.mkdir_p(ssh)
        File.chmod(0o700, ssh)
        private_key = File.join(ssh, AUTO_KEY)
        unless File.exist?(private_key) && File.exist?("#{private_key}.pub")
            FileUtils.rm_f([private_key, "#{private_key}.pub"])
            _, status = Open3.capture2e('ssh-keygen', '-q', '-t', 'ed25519', '-N', '', '-C', 'Workspace-Git', '-f', private_key)
            raise 'ssh-keygen failed' unless status.success?
        end
        config = File.join(ssh, 'config')
        File.write(config, replace_block(File.exist?(config) ? File.read(config) : '', ssh_config_lines))
        known = known_hosts_line(host_key_text)
        if known
            hosts = File.join(ssh, 'known_hosts')
            File.write(hosts, replace_block(File.exist?(hosts) ? File.read(hosts) : '', [known]))
        end
        File.chmod(0o600, private_key)
        File.chmod(0o644, "#{private_key}.pub")
        File.chmod(0o600, config)
        [ssh, private_key, "#{private_key}.pub", config, File.join(ssh, 'known_hosts')].each do |path|
            File.chown(uid, uid, path) if File.exist?(path)
        end
        parse_key(File.read("#{private_key}.pub"))[:line]
    end

    # --- repositories ------------------------------------------------------------

    def self.repo_path(git_id, name, root: REPOS)
        raise ArgumentError, 'git id' unless git_id.to_s.match?(GIT_ID)
        raise ArgumentError, 'name' unless name.to_s.match?(NAME)

        File.join(root, git_id, "#{name}.git")
    end

    def self.git(*args, chdir: nil, env: {})
        options = chdir ? { :chdir => chdir } : {}
        out, status = Open3.capture2e(env, 'git', '-c', 'safe.directory=*', *args, **options)
        raise "git #{args.first} failed: #{out.strip}" unless status.success?

        out
    end

    # A new bare repository, branch main, with REPO_CONFIG; with starter (a
    # folder), a first commit of its files; with readme (a text), a first
    # commit of a README.md; description in git's file of that name. Owned
    # by the server's user.
    def self.create_repo(git_id, name, starter: nil, readme: nil, description: nil, root: REPOS, uid: UID)
        path = repo_path(git_id, name, :root => root)
        raise 'exists' if File.exist?(path)

        FileUtils.mkdir_p(File.dirname(path))
        git('init', '-q', '--bare', '-b', 'main', path)
        REPO_CONFIG.each { |key, value| git('--git-dir', path, 'config', key, value) }
        # No sample hooks: the server runs none.
        FileUtils.rm_rf(File.join(path, 'hooks'))
        FileUtils.mkdir_p(File.join(path, 'hooks'))
        File.write(File.join(path, 'description'), "#{clean_description(description)}\n")
        if (starter && File.directory?(starter)) || readme
            Dir.mktmpdir('git-starter') do |work|
                if readme
                    File.write(File.join(work, 'README.md'), readme)
                else
                    FileUtils.cp_r(File.join(starter, '.'), work)
                end
                author = { 'GIT_AUTHOR_NAME' => STARTER_AUTHOR[0], 'GIT_AUTHOR_EMAIL' => STARTER_AUTHOR[1],
                           'GIT_COMMITTER_NAME' => STARTER_AUTHOR[0], 'GIT_COMMITTER_EMAIL' => STARTER_AUTHOR[1] }
                git('--git-dir', path, '--work-tree', work, 'add', '-A', :env => author)
                git('--git-dir', path, '--work-tree', work, 'commit', '-q', '-m', 'Der Anfang', :env => author)
            end
            FileUtils.rm_f(File.join(path, 'index'))
        end
        FileUtils.chown_R(uid, uid, File.dirname(path))
        path
    end

    def self.delete_repo(git_id, name, root: REPOS)
        FileUtils.rm_rf(repo_path(git_id, name, :root => root))
    end

    def self.delete_folder(git_id, root: REPOS)
        raise ArgumentError, 'git id' unless git_id.to_s.match?(GIT_ID)

        FileUtils.rm_rf(File.join(root, git_id))
    end

    # A repository's description, '' for none.
    def self.description(path)
        text = File.exist?(File.join(path, 'description')) ? File.read(File.join(path, 'description'), 1024).to_s.force_encoding('UTF-8').scrub : ''
        text.match?(NO_DESCRIPTION) ? '' : clean_description(text)
    end

    # The repositories in a folder (a group's or a person's):
    # [{ name:, size:, last: Time or nil, author: the last commit's author's name or nil, description: }].
    # One git call per repository; names only, never an e-mail address.
    def self.repos(git_id, root: REPOS)
        return [] unless git_id.to_s.match?(GIT_ID)

        Dir.glob(File.join(root, git_id, '*.git')).sort.filter_map do |path|
            name = File.basename(path, '.git')
            next unless name.match?(NAME)

            stamp, author = begin
                git('--git-dir', path, 'log', '-1', '--format=%ct%x1f%an', '--all').strip.split("\x1f", 2)
            rescue RuntimeError
                nil
            end
            last = stamp.to_s.empty? ? nil : Time.at(stamp.to_i)
            size = Dir.glob(File.join(path, '**', '*')).sum { |f| File.file?(f) ? File.size(f) : 0 }
            { :name => name, :size => size, :last => last, :author => last ? author.to_s : nil, :description => description(path) }
        end
    end
end
