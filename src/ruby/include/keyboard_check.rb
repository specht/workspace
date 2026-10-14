require 'json'
require 'yaml'

# Check type keyboard (docs/aufgaben.md): one section of the Hackschule
# Keyboard-Tutorial, the VS Code extension in src/vscode-extensions/
# keyboard-tutorial.
#
# The extension writes its progress to ~/.hs-kbd-tutorial/.state.json, in
# the Workspace /workspace/.hs-kbd-tutorial/.state.json: { step key => true
# or false }. The file appears with the first finished step. Which steps belong to a section comes from the extension's
# own tutorial/sections.yaml, the file it is built from. The server only
# reads the state file, like a files check; nothing runs.
#
#   check:
#     type: keyboard
#     section: Text markieren       # a heading in sections.yaml
module KeyboardCheck
    SECTIONS_PATH = File.expand_path('../../vscode-extensions/keyboard-tutorial/tutorial/sections.yaml', __dir__)
    STATE_FILE = '.hs-kbd-tutorial/.state.json'

    # [{ 'heading' => …, 'steps' => [{ 'heading' => …, 'key' => … }] }]
    def self.sections(path = SECTIONS_PATH)
        @sections ||= {}
        mtime = File.mtime(path) rescue nil
        return [] if mtime.nil?

        cached = @sections[path]
        return cached[1] if cached && cached[0] == mtime

        data = YAML.safe_load(File.read(path, encoding: 'UTF-8'), permitted_classes: [], aliases: false)
        list = (data.is_a?(Hash) ? data['sections'] : nil) || []
        @sections[path] = [mtime, list]
        list
    end

    def self.section(heading, path = SECTIONS_PATH)
        sections(path).find { |s| s['heading'] == heading }
    end

    def self.spec_errors(check, path = SECTIONS_PATH)
        headings = sections(path).map { |s| s['heading'] }
        return ["section must be one of: #{headings.join(', ')}"] unless headings.include?(check['section'])

        []
    end

    # The state file as { key => true/false }, or a problem code.
    def self.read_state(workspace)
        return [nil, :missing] if workspace.entry(STATE_FILE)[:type] != :file

        state = JSON.parse(workspace.read_text(STATE_FILE, max: 64 * 1024))
        state.is_a?(Hash) ? [state, nil] : [nil, :unreadable]
    rescue JSON::ParserError
        [nil, :unreadable]
    end

    # [status, problem, message, read]
    def self.evaluate(check, state, problem, path = SECTIONS_PATH)
        section = section(check['section'], path)
        steps = section ? section['steps'] || [] : []
        read = "`/workspace/#{STATE_FILE}`, der Fortschritt des Keyboard-Tutorials"
        case problem
        when :missing
            return [:wrong, :missing,
                'Im Keyboard-Tutorial ist noch kein Schritt abgehakt. Du öffnest es links in der Aktivitätsleiste mit dem Tastatur-Symbol.', read]
        when :unreadable
            return [:wrong, :unreadable,
                "Die Datei `/workspace/#{STATE_FILE}` lässt sich nicht lesen. Öffne das Keyboard-Tutorial einmal, dann schreibt es sie neu.", read]
        end

        done = steps.select { |s| state[s['key']] == true }
        open = steps - done
        read = "#{read}: #{done.size} von #{steps.size} Schritten in »#{check['section']}« erledigt"
        return [:correct, nil, "Alle #{steps.size} Schritte von »#{check['section']}« sind erledigt.", read] if open.empty?

        names = open.map { |s| "»#{s['heading']}«" }
        list = if names.size > 4 then "#{names.first(3).join(', ')} und #{names.size - 3} weitere"
            elsif names.size == 1 then names.first
            else "#{names[0..-2].join(', ')} und #{names.last}"
        end
        [:wrong, :incomplete, "#{done.size} von #{steps.size} Schritten sind erledigt. Noch offen: #{list}.", read]
    end
end
