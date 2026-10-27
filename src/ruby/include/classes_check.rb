require 'base64'
require 'json'
require 'shellwords'
require 'zlib'

# Check type classes (docs/aufgaben.md): the class diagram in a .uml file
# (the UML-Werkstatt, src/vscode-extensions/uml).
#
# Like ErCheck, the server does not read the diagram itself: it sends the
# UML-Werkstatt's core along (gzipped, so that a Workspace with an older
# extension gets the same reading) and runs its command line with node in the
# person's Workspace (node uml-core.js --json file), then compares the summary
# with the task. Everything after that works on the JSON and can be tested
# without a Workspace.
#
#   check:
#     type: classes
#     file: bibliothek/bibliothek.uml
#     classes:
#       - name: Medium
#         abstract: true
#         attributes: ["- title: String", available]   # as in the file; sign and type only if given
#         methods: ["+ lend(): void", "isAvailable(): boolean"]
#       - name: Book
#         extends: Medium
#         methods: ["+ Book(title: String, author: String)"]  # a constructor: the class's name, no type
#     associations:
#       - between: [Member, Medium]
#         multiplicities: ["1", "0..*"]    # in the order of between
#         directed: false                  # optional; true: Member -> Medium
#         name: loans                      # optional
#
# What the task names must be there; more classes, members and associations
# may be. A class has what it inherits too (a Book that extends Medium has
# Medium's title), but not its parent's constructors. A method is matched by
# its name and, if given, the types of its parameters (their names do not
# count) and its return type.
module ClassesCheck
    CORE_PATH = File.expand_path('../../vscode-extensions/uml/core/uml-core.js', __dir__)
    CORE = File.file?(CORE_PATH) ? Base64.strict_encode64(Zlib.gzip(File.read(CORE_PATH, encoding: 'UTF-8'), :level => 9)) : nil
    NAME = /\A[A-Za-z][A-Za-z0-9_]*\z/
    MULTIPLICITY = /\A(\*|\d+|\d+\.\.(\d+|\*))\z/
    PROBLEMS = %w[missing errors missing_class abstract extends missing_attribute attribute missing_method method
                  missing_association multiplicity direction].freeze

    def self.available?
        !CORE.nil?
    end

    # The script that prints the summary, run in /workspace as the person.
    def self.command(file)
        <<~SH
            t=$(mktemp -d) || exit 1
            trap 'rm -rf "$t"' EXIT
            printf '%s' #{Shellwords.escape(CORE)} | base64 -d | gunzip > "$t/uml-core.js"
            node "$t/uml-core.js" --json #{Shellwords.escape(file)}
        SH
    end

    def self.parse(output)
        data = JSON.parse(output.to_s)
        data.is_a?(Hash) ? data : nil
    rescue JSON::ParserError
        nil
    end

    def self.code(text)
        "`#{text}`"
    end

    def self.list(names)
        names = names.map { |n| code(n) }
        names.size <= 1 ? names.join : "#{names[0..-2].join(', ')} und #{names[-1]}"
    end

    def self.squeeze(type)
        type.to_s.gsub(/\s+/, '').gsub(',', ', ')
    end

    # The diagram uses the names of the student's language: isAvailable,
    # is_available and available? (Ruby) are one method, loanDays and
    # loan_days one, so a task asks in any spelling. A »!« stays: sort! and
    # sort are two methods in Ruby.
    def self.same_name(name)
        name = name.to_s
        name = "is#{name.chomp('?')}" if name.end_with?('?')
        name.delete('_').downcase
    end

    # "- title: String" -> { visibility: '-', name: 'title', type: 'String' }
    def self.parse_attribute(text)
        m = text.to_s.strip.match(/\A([+-])?\s*([A-Za-z][A-Za-z0-9_]*)\s*(?::\s*(.+))?\z/)
        return nil unless m

        { :visibility => m[1], :name => m[2], :type => m[3] && squeeze(m[3]) }
    end

    # "+ borrow(medium: Medium): void" -> { visibility:, name:, params: ['Medium'] or nil, returns: 'void' or nil }
    def self.parse_method(text)
        m = text.to_s.strip.match(/\A([+-])?\s*([A-Za-z][A-Za-z0-9_]*[?!]?)\s*(?:\((.*)\))?\s*(?::\s*(.+))?\z/)
        return nil unless m

        params = m[3]&.then do |inside|
            depth = 0
            pieces = ['']
            inside.each_char do |c|
                depth += 1 if c == '<'
                depth -= 1 if c == '>'
                c == ',' && depth.zero? ? pieces << '' : pieces[-1] += c
            end
            pieces.map(&:strip).reject(&:empty?).map { |p| squeeze(p.include?(':') ? p.split(':', 2)[1] : p) }
        end
        { :visibility => m[1], :name => m[2], :params => params, :returns => m[4] && squeeze(m[4]) }
    end

    def self.spec_errors(check)
        errors = []
        errors << 'classes needs classes or associations' unless %w[classes associations].any? { |k| check.key?(k) }
        if check.key?('classes')
            list = check['classes']
            if list.is_a?(Array) && !list.empty?
                list.each_with_index do |c, i|
                    c = { 'name' => c } if c.is_a?(String)
                    unless c.is_a?(Hash) && c['name'].to_s =~ NAME
                        errors << "classes[#{i}] needs a name"
                        next
                    end
                    extra = c.keys - %w[name abstract extends attributes methods]
                    errors << "classes[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    errors << "classes[#{i}].abstract must be true or false" if c.key?('abstract') && ![true, false].include?(c['abstract'])
                    errors << "classes[#{i}].extends must be a class name or ~" if c.key?('extends') && !(c['extends'].nil? || c['extends'].to_s =~ NAME)
                    %w[attributes methods].each do |key|
                        next unless c.key?(key)

                        unless c[key].is_a?(Array) && !c[key].empty?
                            errors << "classes[#{i}].#{key} must be a list"
                            next
                        end
                        c[key].each do |text|
                            parsed = key == 'attributes' ? parse_attribute(text) : parse_method(text)
                            errors << "classes[#{i}].#{key}: cannot read »#{text}«" if parsed.nil?
                        end
                    end
                end
            else
                errors << 'classes must be a list'
            end
        end
        if check.key?('associations')
            list = check['associations']
            if list.is_a?(Array) && !list.empty?
                list.each_with_index do |a, i|
                    unless a.is_a?(Hash) && a['between'].is_a?(Array) && a['between'].size == 2 && a['between'].all? { |n| n.to_s =~ NAME }
                        errors << "associations[#{i}] needs between: [Class, Class]"
                        next
                    end
                    extra = a.keys - %w[between multiplicities directed name]
                    errors << "associations[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    if a.key?('multiplicities') && !(a['multiplicities'].is_a?(Array) && a['multiplicities'].size == 2 && a['multiplicities'].all? { |m| m.to_s =~ MULTIPLICITY })
                        errors << "associations[#{i}].multiplicities must be two, like [\"1\", \"0..*\"]"
                    end
                    errors << "associations[#{i}].directed must be true or false" if a.key?('directed') && ![true, false].include?(a['directed'])
                end
            else
                errors << 'associations must be a list'
            end
        end
        errors
    end

    def self.member_text(m)
        if m.key?('params')
            params = (m['params'] || []).map { |p| "#{p['name']}: #{p['type']}" }.join(', ')
            "#{m['visibility']} #{m['name']}(#{params})#{m['returns'] ? ": #{m['returns']}" : ''}"
        else
            "#{m['visibility']} #{m['name']}: #{m['type']}"
        end
    end

    # [ok, problem, message] for the summary.
    def self.match(check, summary, file)
        errors = summary['errors'] || []
        unless errors.empty?
            first = errors.first(3).map { |p| "- Zeile #{p['line']}: #{p['msg']}" }.join("\n")
            more = errors.size > 3 ? "\n- … und #{errors.size - 3} weitere" : ''
            return [false, :errors, "In #{code(file)} #{errors.size == 1 ? 'steht noch ein Fehler' : "stehen noch #{errors.size} Fehler"}. Die UML-Werkstatt unterstreicht sie auch im Editor.\n\n#{first}#{more}"]
        end

        classes = (summary['classes'] || []).to_h { |c| [c['name'], c] }
        (check['classes'] || []).each do |spec|
            spec = { 'name' => spec } if spec.is_a?(String)
            name = spec['name']
            have = classes[name]
            if have.nil?
                other = classes.keys.find { |n| n.downcase == name.downcase }
                hint = other ? " Es gibt #{code(other)}; Groß- und Kleinschreibung zählen." : ''
                return [false, :missing_class, "Es fehlt die Klasse #{code(name)}.#{hint}"]
            end
            if spec.key?('abstract') && !!have['abstract'] != spec['abstract']
                message = spec['abstract'] ? "#{code(name)} soll abstrakt sein: Schreib `(abstract)` hinter `class #{name}`." : "#{code(name)} soll nicht abstrakt sein."
                return [false, :abstract, message]
            end
            if spec.key?('extends') && have['extends'] != spec['extends']
                message = if spec['extends'].nil?
                    "#{code(name)} soll von keiner anderen Klasse erben."
                elsif have['extends'].nil?
                    "#{code(name)} soll von #{code(spec['extends'])} erben: `class #{name} extends #{spec['extends']}`."
                else
                    "#{code(name)} erbt von #{code(have['extends'])}, gefragt ist #{code(spec['extends'])}."
                end
                return [false, :extends, message]
            end
            line = ancestry(have, classes)
            attributes = line.reverse.flat_map { |c| c['attributes'] || [] }.to_h { |a| [same_name(a['name']), a] }
            (spec['attributes'] || []).each do |text|
                want = parse_attribute(text)
                got = attributes[same_name(want[:name])]
                if got.nil?
                    there = attributes.empty? ? 'noch keine Attribute' : "nur #{list(attributes.values.map { |a| a['name'] })}"
                    return [false, :missing_attribute, "#{code(name)} hat noch kein Attribut #{code(want[:name])}. Es hat #{there}."]
                end
                if want[:type] && squeeze(got['type']) != want[:type]
                    return [false, :attribute, "#{code("#{name}.#{want[:name]}")} hat den Typ #{code(got['type'])}, gefragt ist #{code(want[:type])}."]
                end
                if want[:visibility] && got['visibility'] != want[:visibility]
                    said = want[:visibility] == '-' ? 'privat sein (`-`)' : 'öffentlich sein (`+`)'
                    return [false, :attribute, "#{code("#{name}.#{want[:name]}")} soll #{said}."]
                end
            end
            # its own methods first, then what it inherits, constructors not
            methods = (have['methods'] || []) + line.drop(1).flat_map { |c| (c['methods'] || []).reject { |m| m['constructor'] } }
            (spec['methods'] || []).each do |text|
                want = parse_method(text)
                named = methods.select { |m| same_name(m['name']) == same_name(want[:name]) }
                what = want[:name] == name ? 'keinen Konstruktor' : "keine Methode #{code(want[:name])}"
                return [false, :missing_method, "#{code(name)} hat noch #{what}."] if named.empty?

                fitting = want[:params] ? named.select { |m| (m['params'] || []).map { |p| squeeze(p['type']) } == want[:params] } : named
                if fitting.empty?
                    shown = named.first
                    return [false, :method, "#{code(member_text(shown).strip)} bekommt andere Parameter als gefragt: #{want[:params].empty? ? 'keine' : list(want[:params])}."]
                end
                got = fitting.first
                if want[:returns] && squeeze(got['returns']) != want[:returns]
                    return [false, :method, "#{code("#{name}.#{want[:name]}")} gibt #{code(got['returns'] || 'nichts')} zurück, gefragt ist #{code(want[:returns])}."]
                end
                if want[:visibility] && got['visibility'] != want[:visibility]
                    said = want[:visibility] == '-' ? 'privat sein (`-`)' : 'öffentlich sein (`+`)'
                    return [false, :method, "#{code("#{name}.#{want[:name]}")} soll #{said}."]
                end
            end
        end

        associations = summary['associations'] || []
        (check['associations'] || []).each do |spec|
            a, b = spec['between']
            hits = associations.select { |s| [s['from'], s['to']].sort == [a, b].sort }
            hits = hits.select { |s| s['name'] == spec['name'] } if spec['name'] && hits.any? { |s| s['name'] == spec['name'] }
            return [false, :missing_association, "Es fehlt eine Assoziation zwischen #{code(a)} und #{code(b)}."] if hits.empty?

            if spec['multiplicities']
                want = spec['multiplicities'].map(&:to_s)
                found = hits.find { |s| oriented(s, a) == want }
                unless found
                    have = oriented(hits.first, a)
                    return [false, :multiplicity, "Zwischen #{code(a)} und #{code(b)} stehen die Multiplizitäten #{code(have[0])} und #{code(have[1])}, gefragt sind #{code(want[0])} bei #{code(a)} und #{code(want[1])} bei #{code(b)}."]
                end
                hits = [found]
            end
            if spec.key?('directed')
                s = hits.first
                if spec['directed'] && !(s['directed'] && s['from'] == a)
                    return [false, :direction, "Die Assoziation zwischen #{code(a)} und #{code(b)} soll von #{code(a)} aus gerichtet sein: `#{a} … -> … #{b}`."]
                end
                if !spec['directed'] && s['directed']
                    return [false, :direction, "Die Assoziation zwischen #{code(a)} und #{code(b)} soll ungerichtet sein (`--`)."]
                end
            end
        end
        [true, nil, nil]
    end

    # The class and the classes it inherits from, nearest first; a cycle (an
    # error the summary reports anyway) ends the line.
    def self.ancestry(klass, classes)
        line = [klass]
        while (parent = classes[line.last['extends']]) && !line.include?(parent)
            line << parent
        end
        line
    end

    # The multiplicities of an association read in the order [a, other].
    def self.oriented(association, a)
        pair = [association['fromMult'].to_s, association['toMult'].to_s]
        association['from'] == a ? pair : pair.reverse
    end

    # Markdown: what the diagram really contains, in its own notation.
    def self.describe(summary, file)
        return "#{code(file)} gibt es nicht." if summary['missing']

        out = ["Gelesen: #{code(file)}"]
        classes = summary['classes'] || []
        classes.each do |c|
            head = "class #{c['name']}#{c['extends'] ? " extends #{c['extends']}" : ''}#{c['abstract'] ? ' (abstract)' : ''}"
            members = (c['attributes'] || []) + (c['methods'] || [])
            out << '' << '```' << head
            members.each { |m| out << "  #{member_text(m)}" }
            out << '```'
        end
        associations = summary['associations'] || []
        unless associations.empty?
            out << '' << '```'
            associations.each { |s| out << "association #{s['name']}: #{s['from']} #{s['fromMult']} #{s['directed'] ? '->' : '--'} #{s['toMult']} #{s['to']}" }
            out << '```'
        end
        out << '' << 'Das Diagramm ist noch leer.' if classes.empty? && associations.empty?
        out.join("\n")
    end

    # [status, problem, message, read] (TaskCheck turns it into a Result).
    def self.evaluate(task, check, summary)
        file = check['file']
        return [:unavailable, nil, 'Die UML-Werkstatt hat das Diagramm nicht gelesen. Versuch es gleich noch einmal.', nil] if summary.nil?

        read = describe(summary, file)
        return [:wrong, :missing, "#{code(file)} gibt es nicht.", read] if summary['missing']

        ok, problem, message = match(check, summary, file)
        return [:correct, nil, 'Passt.', read] if ok

        say = (task['feedback'] || []).find { |entry| entry['when']['problem'].to_s == problem.to_s }
        [:wrong, problem, say ? say['say'] : message, read]
    end
end
