require 'fileutils'
require 'find'
require 'yaml'

module TestWorkspacePackage
    class ConfigError < StandardError; end

    CONFIG_RELATIVE_PATH = '.workspace/config.yaml'
    DEFAULT_EXAM_COLOR = 'blue'
    DEFAULT_GIT_MODE = 'fresh'
    GIT_MODES = ['fresh', 'preserve', 'none'].freeze

    # Built-in VS Code themes cover the editor, welcome page, panels and chrome.
    EXAM_COLOR_THEMES = {
        'blue' => 'Tomorrow Night Blue',
        'red' => 'Red',
    }.freeze

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

    def self.vscode_config(workspace_path)
        config = load_config(workspace_path)
        package_settings = config['vscode_config'] || {}
        unless package_settings.is_a?(Hash)
            raise ConfigError, 'vscode_config muss ein YAML-Objekt enthalten.'
        end

        exam_config = config['exam'] || {}
        raise ConfigError, 'exam muss ein YAML-Objekt enthalten.' unless exam_config.is_a?(Hash)

        color = (exam_config['color'] || DEFAULT_EXAM_COLOR).to_s
        unless EXAM_COLOR_THEMES.key?(color)
            raise ConfigError, "Unbekannte exam.color-Farbe #{color.inspect}; erlaubt sind blue und red."
        end

        package_customizations = package_settings['workbench.colorCustomizations']
        unless package_customizations.nil? || package_customizations.is_a?(Hash)
            raise ConfigError, 'workbench.colorCustomizations muss ein Objekt enthalten.'
        end

        result = package_settings.dup
        # An exam's own theme wins over the regular Workspace's persisted theme.
        result['workbench.colorTheme'] = EXAM_COLOR_THEMES[color]
        result['window.autoDetectColorScheme'] = false
        # Clear old exam-specific accent overrides instead of carrying them
        # into the complete built-in theme. Keep explicitly supplied overrides.
        result['workbench.colorCustomizations'] = package_customizations || {}
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

    def self.write_git_exclude(workspace_path)
        path = File.join(workspace_path, '.git', 'info', 'exclude')
        FileUtils.mkdir_p(File.dirname(path))
        existing = File.exist?(path) ? File.read(path) : ''
        marker = '# Hackschule exam workspace'
        return path if existing.include?(marker)

        File.open(path, 'a') do |f|
            f.puts unless existing.empty? || existing.end_with?("\n")
            f.puts marker
            INTERNAL_GIT_EXCLUDES.each { |entry| f.puts entry }
        end
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
