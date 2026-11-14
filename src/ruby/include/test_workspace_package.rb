require 'fileutils'
require 'find'
require 'yaml'
require_relative 'user_files'

module TestWorkspacePackage
    class ConfigError < StandardError; end

    CONFIG_RELATIVE_PATH = '.workspace/config.yaml'
    DEFAULT_EXAM_COLOR = 'blue'
    DEFAULT_GIT_MODE = 'fresh'
    GIT_MODES = ['fresh', 'preserve', 'none'].freeze

    # exam.look: »theme« (the default, as before) colours everything with a
    # built-in VS Code theme, editor, welcome page and panels included;
    # »frame« colours the frame around the editor (title bar, the strip on
    # the left, the tabs, the status bar) in a signal colour seen from the
    # back of the room, and the editor keeps a normal dark theme, easy to
    # read for an hour and a half. Two groups side by side (A and B) can
    # have two colours.
    EXAM_LOOKS = ['theme', 'frame'].freeze
    DEFAULT_EXAM_LOOK = 'theme'
    EXAM_COLOR_THEMES = {
        'blue' => 'Tomorrow Night Blue',
        'red' => 'Red',
    }.freeze
    # the frame's colour and the text on it
    EXAM_FRAME_COLORS = {
        'yellow' => ['#ffd60a', '#1b1b1b'],
        'orange' => ['#ff7a00', '#1b1b1b'],
        'magenta' => ['#d6007a', '#ffffff'],
        'green' => ['#119c4a', '#ffffff'],
        'blue' => ['#1f6feb', '#ffffff'],
        'red' => ['#d42a2a', '#ffffff'],
    }.freeze
    FRAME_EDITOR_THEME = 'Default Dark Modern'

    def self.frame_customizations(color)
        bg, fg = EXAM_FRAME_COLORS.fetch(color)
        dim = "#{fg}b3"
        {
            'titleBar.activeBackground' => bg, 'titleBar.activeForeground' => fg,
            'titleBar.inactiveBackground' => bg, 'titleBar.inactiveForeground' => dim,
            'titleBar.border' => bg,
            'activityBar.background' => bg, 'activityBar.foreground' => fg,
            'activityBar.inactiveForeground' => dim, 'activityBar.activeBorder' => fg,
            'activityBar.border' => bg,
            'activityBarBadge.background' => fg, 'activityBarBadge.foreground' => bg,
            'editorGroupHeader.tabsBackground' => bg, 'editorGroupHeader.tabsBorder' => bg,
            'tab.inactiveBackground' => bg, 'tab.inactiveForeground' => fg, 'tab.border' => bg,
            'statusBar.background' => bg, 'statusBar.foreground' => fg,
            'statusBar.noFolderBackground' => bg, 'statusBar.debuggingBackground' => bg,
            'statusBar.debuggingForeground' => fg, 'statusBar.border' => bg,
            'statusBarItem.remoteBackground' => bg, 'statusBarItem.remoteForeground' => fg,
        }
    end

    INTERNAL_GIT_EXCLUDES = [
        '.workspace/',
        '.exam-extensions/',
        '.cache/',
        '.config/',
        '.extensions/',
        '.local/',
        '.ssh/',
        '.bash_history',
        '.bashrc',
        '.profile',
        '.hackschule',
        '.gitconfig',
        '.my.cnf',
        '.myclirc',
        '.test_init',
        '.test_git_pending',
        '.test_git_init',
    ].freeze

    INTERNAL_PRINT_EXCLUDES = [
        '.git/**',
        '.workspace/**',
        '.cache/**',
        '.config/**',
        '.extensions/**',
        '.local/**',
        '.ssh/**',
        '.bash_history',
        '.bashrc',
        '.profile',
        '.hackschule',
        '.gitconfig',
        '.my.cnf',
        '.myclirc',
        '.test_init',
        '.test_git_pending',
        '.test_git_init',
    ].freeze

    FNM_FLAGS = File::FNM_PATHNAME | File::FNM_DOTMATCH | File::FNM_EXTGLOB

    def self.load_config(workspace_path)
        path = File.join(workspace_path, CONFIG_RELATIVE_PATH)
        return {} unless File.file?(path)

        config = YAML.safe_load(
            File.read(path),
            :permitted_classes => [],
            :permitted_symbols => [],
            :aliases => false,
        )
        config ||= {}
        raise ConfigError, "#{CONFIG_RELATIVE_PATH} muss ein YAML-Objekt enthalten." unless config.is_a?(Hash)

        version = config['workspace_package']
        if version && ![1, '1'].include?(version)
            raise ConfigError, "Nicht unterstützte workspace_package-Version: #{version.inspect}"
        end

        config
    rescue Psych::Exception => e
        raise ConfigError, "#{CONFIG_RELATIVE_PATH} ist kein gültiges YAML: #{e.message}"
    end

    # Exam setup runs as the workspace user, with HOME and cwd set to /workspace.
    # Upload-time validation keeps invalid setup from reaching any student.
    def self.setup_commands(workspace_path)
        exam = load_config(workspace_path).fetch('exam', {})
        raise ConfigError, 'exam muss ein YAML-Objekt enthalten.' unless exam.is_a?(Hash)

        setup = exam.fetch('setup', {})
        raise ConfigError, 'exam.setup muss ein YAML-Objekt enthalten.' unless setup.is_a?(Hash)
        unknown = setup.keys - ['once', 'every_start']
        unless unknown.empty?
            raise ConfigError, "Unbekannte exam.setup-Optionen: #{unknown.join(', ')}"
        end

        result = {}
        ['once', 'every_start'].each do |kind|
            commands = setup.fetch(kind, [])
            unless commands.is_a?(Array) && commands.length <= 12 && commands.all? { |command|
                    command.is_a?(String) && !command.strip.empty? &&
                        command.bytesize <= 2048 && !command.include?("\0")
                }
                raise ConfigError,
                    "exam.setup.#{kind} muss eine Liste von höchstens 12 nichtleeren Befehlen (je maximal 2048 Bytes) sein."
            end
            result[kind] = commands
        end
        result
    end

    def self.git_mode(workspace_path)
        config = load_config(workspace_path)
        git_config = config['git'] || {}
        raise ConfigError, 'git muss ein YAML-Objekt enthalten.' unless git_config.is_a?(Hash)

        mode = (git_config['mode'] || DEFAULT_GIT_MODE).to_s
        unless GIT_MODES.include?(mode)
            raise ConfigError, "Unbekannter git.mode #{mode.inspect}; erlaubt sind fresh, preserve und none."
        end
        mode
    end

    def self.validate_git_layout!(workspace_path)
        mode = git_mode(workspace_path)
        git_path = File.join(workspace_path, '.git')
        has_git = File.directory?(git_path) && !File.symlink?(git_path)
        suspicious_git = File.exist?(git_path) && !has_git

        if suspicious_git
            raise ConfigError, '.git muss ein normales Verzeichnis sein.'
        end

        case mode
        when 'fresh'
            if has_git
                raise ConfigError,
                    'Das Paket enthält ein .git-Verzeichnis. Verwende git.mode: preserve, ' \
                    'wenn der vorhandene Git-Verlauf Teil der Leistungsüberprüfung ist, ' \
                    'oder entferne .git aus dem Paket.'
            end
        when 'preserve'
            unless has_git
                raise ConfigError,
                    'git.mode: preserve benötigt ein .git-Verzeichnis im Paket.'
            end
        when 'none'
            if has_git
                raise ConfigError,
                    'git.mode: none erwartet ein Paket ohne .git-Verzeichnis. ' \
                    'Verwende git.mode: preserve, um ein vorhandenes Repository zu behalten.'
            end
        end

        mode
    end

    # network.allow: what an exam Workspace may reach outside the students'
    # network, which it otherwise cannot (no DNS, and peer_firewall drops
    # everything else that leaves it): a service the class examines during
    # the exam. Each entry a name or IPv4 address, with :port (without, 80
    # and 443); at most MAX_ALLOWED. Read at upload and locked in the
    # manifest (TestWorkspaceExtensions), never from the student's copy.
    MAX_ALLOWED = 10
    HOST_NAME = /\A(?=.{1,253}\z)([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)(\.[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)*\z/

    def self.network_allow(workspace_path)
        network = load_config(workspace_path)['network'] || {}
        raise ConfigError, 'network muss ein YAML-Objekt enthalten.' unless network.is_a?(Hash)
        unknown = network.keys - ['allow']
        raise ConfigError, "Unbekannte network-Optionen: #{unknown.join(', ')}" unless unknown.empty?

        entries = network['allow'] || []
        unless entries.is_a?(Array) && entries.size <= MAX_ALLOWED
            raise ConfigError, "network.allow muss eine Liste von höchstens #{MAX_ALLOWED} Einträgen sein."
        end
        entries.map do |entry|
            host, port = entry.to_s.strip.downcase.split(':', 2)
            ports = port.nil? ? [80, 443] : [port.to_i]
            unless host.to_s.match?(HOST_NAME) && (port.nil? || (port.match?(/\A\d{1,5}\z/) && ports.first.between?(1, 65_535)))
                raise ConfigError, "network.allow: #{entry.inspect} ist kein Name oder keine IPv4-Adresse mit optionalem :Port."
            end
            { 'host' => host, 'ports' => ports }
        end
    end

    # Repositories in subfolders (a .git folder below the top), part of a
    # Git task: the recovery repository of fresh leaves them out, and with
    # fresh and preserve they get the person's name and address too
    # (initialize_test_git). Not through links, not into hidden folders,
    # at most MAX_NESTED_REPOSITORIES, at most six levels deep.
    MAX_NESTED_REPOSITORIES = 20

    def self.nested_repositories(workspace_path)
        root = File.expand_path(workspace_path)
        found = []
        Find.find(root) do |path|
            next if path == root
            Find.prune if File.symlink?(path) || !File.directory?(path)
            rel = path.delete_prefix("#{root}/")
            Find.prune if File.basename(path).start_with?('.') || rel.count('/') >= 6
            git = File.join(path, '.git')
            if File.directory?(git) && !File.symlink?(git)
                found << rel
                break if found.size >= MAX_NESTED_REPOSITORIES
            end
        end
        found.sort
    end

    def self.vscode_config(workspace_path)
        config = load_config(workspace_path)
        package_settings = config['vscode_config'] || {}
        unless package_settings.is_a?(Hash)
            raise ConfigError, 'vscode_config muss ein YAML-Objekt enthalten.'
        end

        exam_config = config['exam'] || {}
        raise ConfigError, 'exam muss ein YAML-Objekt enthalten.' unless exam_config.is_a?(Hash)

        look = (exam_config['look'] || DEFAULT_EXAM_LOOK).to_s
        unless EXAM_LOOKS.include?(look)
            raise ConfigError, "Unbekanntes exam.look #{look.inspect}; erlaubt sind theme und frame."
        end
        color = (exam_config['color'] || DEFAULT_EXAM_COLOR).to_s
        if look == 'theme' && !EXAM_COLOR_THEMES.key?(color)
            hint = EXAM_FRAME_COLORS.key?(color) ? " #{color} gibt es als Rahmen: exam.look: frame." : ''
            raise ConfigError, "Unbekannte exam.color-Farbe #{color.inspect}; erlaubt sind blue und red.#{hint}"
        end
        if look == 'frame' && !EXAM_FRAME_COLORS.key?(color)
            raise ConfigError, "Unbekannte exam.color-Farbe #{color.inspect} für den Rahmen; erlaubt sind #{EXAM_FRAME_COLORS.keys.join(', ')}."
        end

        package_customizations = package_settings['workbench.colorCustomizations']
        unless package_customizations.nil? || package_customizations.is_a?(Hash)
            raise ConfigError, 'workbench.colorCustomizations muss ein Objekt enthalten.'
        end

        result = package_settings.dup
        result['window.autoDetectColorScheme'] = false
        if look == 'theme'
            # An exam's own theme wins over the regular Workspace's persisted theme.
            result['workbench.colorTheme'] = EXAM_COLOR_THEMES[color]
            # Clear old exam-specific accent overrides instead of carrying them
            # into the complete built-in theme. Keep explicitly supplied overrides.
            result['workbench.colorCustomizations'] = package_customizations || {}
        else
            # the frame over a dark editor; a package's own theme and
            # colours win
            result['workbench.colorTheme'] = package_settings['workbench.colorTheme'] || FRAME_EDITOR_THEME
            result['workbench.colorCustomizations'] = frame_customizations(color).merge(package_customizations || {})
        end
        result
    end

    def self.printable_files(workspace_path)
        config = load_config(workspace_path)
        print_config = config['print'] || {}
        raise ConfigError, 'print muss ein YAML-Objekt enthalten.' unless print_config.is_a?(Hash)

        include_patterns = patterns(print_config['include'])
        exclude_patterns = INTERNAL_PRINT_EXCLUDES + patterns(print_config['exclude'])

        workspace_path = File.expand_path(workspace_path)
        files = []
        Find.find(workspace_path) do |path|
            next if path == workspace_path
            relative_path = path.delete_prefix("#{workspace_path}/")

            # Do not traverse hidden directories (including nested .cache,
            # .git, .workspace, etc.) or follow symlinks into other trees.
            next if File.symlink?(path)
            if File.directory?(path)
                Find.prune if File.basename(path).start_with?('.') ||
                    matches_any?(exclude_patterns, relative_path)
                next
            end

            next unless File.file?(path)
            next if matches_any?(exclude_patterns, relative_path)
            next unless include_patterns.empty? || matches_any?(include_patterns, relative_path)
            files << [relative_path, path] if text_file?(path)
        end
        files.sort_by(&:first)
    end

    # Written as root into the exam workspace, so only through UserFiles: a
    # link in .git (the package or the student may have put one there) is
    # refused instead of followed.
    def self.write_git_exclude(workspace_path, extra: [])
        rel = '.git/info/exclude'
        existing = UserFiles.read(workspace_path, rel) || ''
        marker = '# Hackschule exam workspace'
        path = File.join(workspace_path, rel)
        return path if existing.include?(marker)

        text = +''
        text << "\n" unless existing.empty? || existing.end_with?("\n")
        text << "#{marker}\n"
        INTERNAL_GIT_EXCLUDES.each { |entry| text << "#{entry}\n" }
        extra.each { |entry| text << "#{entry}\n" }
        UserFiles.write(workspace_path, rel, text, mode: :append)
        path
    end

    def self.text_file?(path)
        contents = File.binread(path)
        return false if contents.include?("\0")

        contents.force_encoding(Encoding::UTF_8).valid_encoding?
    rescue
        false
    end
    private_class_method :text_file?

    def self.patterns(value)
        return [] if value.nil?
        values = value.is_a?(Array) ? value : [value]
        values.map do |pattern|
            raise ConfigError, 'print-Muster müssen Zeichenketten sein.' unless pattern.is_a?(String)
            pattern
        end
    end
    private_class_method :patterns

    def self.matches_any?(patterns, relative_path)
        patterns.any? do |pattern|
            File.fnmatch?(pattern, relative_path, FNM_FLAGS) ||
                # File.fnmatch? with FNM_PATHNAME does not let a trailing **
                # match multiple directory levels. Treat a final /** as a tree.
                (pattern.end_with?('/**') && (
                    File.fnmatch?(pattern.delete_suffix('/**'), relative_path, FNM_FLAGS) ||
                    File.fnmatch?("#{pattern}/*", relative_path, FNM_FLAGS)
                ))
        end
    end
    private_class_method :matches_any?
end
