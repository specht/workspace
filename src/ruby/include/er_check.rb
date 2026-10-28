require 'base64'
require 'json'
require 'shellwords'
require 'zlib'
require_relative 'table_expect'

# Check type er (docs/aufgaben.md): an ER model in a .er file.
#
# The server does not interpret the model itself. It runs the ER-Werkstatt
# core (src/vscode-extensions/er/core/er-core.js, the same file the VS Code
# extension uses) with node in the person's Workspace and compares the JSON
# summary with the task. Everything after that works on the JSON and can be
# tested without a Workspace.
#
#   check:
#     type: er
#     file: filmdatenbank/film.er
#     entities:
#       - name: movie
#         key: [id]
#         attributes: [title, "year: int, optional"]   # like in the .er file
#     relationships:
#       - between: [movie, genre]
#         cardinality: n:m
#       - between: [movie, crew, job]
#         table: movie_crew
#     tables: [...]            # what the model translates to (TableExpect)
module ErCheck
    CORE_PATH = File.expand_path('../../vscode-extensions/er/core/er-core.js', __dir__)
    CORE = File.file?(CORE_PATH) ? Base64.strict_encode64(Zlib.gzip(File.read(CORE_PATH, encoding: 'UTF-8'), :level => 9)) : nil
    SUMMARY_JS = <<~JS.gsub(/\s*\n\s*/, ' ')
        eval(require('zlib').gunzipSync(Buffer.from(process.argv[1], 'base64')).toString());
        const core = module.exports;
        let text = null;
        try { text = require('fs').readFileSync(process.argv[2], 'utf8'); } catch (e) {}
        if (text === null) { process.stdout.write(JSON.stringify({ missing: true })); }
        else { const c = core.compile(text); process.stdout.write(JSON.stringify(core.summary(c.model, c.result))); }
    JS
    TYPES = /\A(int|text|float|date|datetime|bool|varchar\(\d+\)|decimal\(\d+,\s*\d+\))\z/
    FLAGS = %w[key optional unique].freeze
    PROBLEMS = %w[missing errors missing_entity key missing_attribute attribute missing_relationship cardinality
                  missing_table missing_column absent_column primary_key foreign_key].freeze

    def self.available?
        !CORE.nil?
    end

    # The command that prints the summary, run in /workspace as the person.
    def self.command(file)
        Shellwords.join(['node', '-e', SUMMARY_JS, CORE, file])
    end

    def self.parse(output)
        data = JSON.parse(output.to_s)
        data.is_a?(Hash) ? data : nil
    rescue JSON::ParserError
        nil
    end

    # "year: int, optional" -> { name: 'year', type: 'int', flags: ['optional'] }
    def self.parse_attribute(text)
        name, rest = text.to_s.split(':', 2)
        # »primary key« and the older »key« mean the same.
        parts = rest.to_s.split(/,(?![^(]*\))/).map { |p| p.strip.downcase.sub(/\Aprimary\s+key\z/, 'key') }.reject(&:empty?)
        type = parts.first && !FLAGS.include?(parts.first) ? parts.shift.gsub(/\s+/, '') : nil
        { :name => name.to_s.strip, :type => type, :flags => parts }
    end

    def self.attribute_errors(text)
        a = parse_attribute(text)
        return ["attribute »#{text}« needs a name"] unless a[:name] =~ TableExpect::NAME
        return ["attribute »#{text}«: unknown type #{a[:type]}"] if a[:type] && a[:type] !~ TYPES
        return ["attribute »#{text}«: flags are #{FLAGS.join(', ')}"] unless (a[:flags] - FLAGS).empty?

        []
    end

    # Spec errors (TaskSpec).
    def self.spec_errors(check)
        errors = []
        unless %w[entities relationships tables].any? { |k| check.key?(k) }
            errors << 'er needs entities, relationships or tables'
        end
        if check.key?('entities')
            list = check['entities']
            if list.is_a?(Array) && !list.empty?
                list.each_with_index do |e, i|
                    e = { 'name' => e } if e.is_a?(String)
                    unless e.is_a?(Hash) && e['name'].to_s =~ TableExpect::NAME
                        errors << "entities[#{i}] needs a name"
                        next
                    end
                    extra = e.keys - %w[name key attributes]
                    errors << "entities[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    errors << "entities[#{i}].key must be a list of attribute names" if e.key?('key') && !(e['key'].is_a?(Array) && !e['key'].empty? && e['key'].all? { |k| k.to_s =~ TableExpect::NAME })
                    if e.key?('attributes')
                        if e['attributes'].is_a?(Array) && !e['attributes'].empty?
                            e['attributes'].each { |a| errors.concat(attribute_errors(a).map { |m| "entities[#{i}]: #{m}" }) }
                        else
                            errors << "entities[#{i}].attributes must be a list"
                        end
                    end
                end
            else
                errors << 'entities must be a list'
            end
        end
        if check.key?('relationships')
            list = check['relationships']
            if list.is_a?(Array) && !list.empty?
                list.each_with_index do |r, i|
                    unless r.is_a?(Hash) && r['between'].is_a?(Array) && r['between'].size >= 2 && r['between'].all? { |n| n.to_s =~ TableExpect::NAME }
                        errors << "relationships[#{i}] needs between: [entity, entity, …]"
                        next
                    end
                    extra = r.keys - %w[between cardinality name table]
                    errors << "relationships[#{i}]: unknown keys: #{extra.join(', ')}" unless extra.empty?
                    if r.key?('cardinality')
                        errors << "relationships[#{i}].cardinality only for two entities" if r['between'].size != 2
                        errors << "relationships[#{i}].cardinality must be 1:1, 1:n, n:1 or n:m" unless r['cardinality'].to_s =~ /\A[1nm]:[1nm]\z/
                    end
                    errors << "relationships[#{i}].table must be a table name" if r.key?('table') && r['table'].to_s !~ TableExpect::NAME
                end
            else
                errors << 'relationships must be a list'
            end
        end
        errors.concat(TableExpect.spec_errors(check['tables'], :rows => false)) if check.key?('tables')
        errors
    end

    def self.code(name)
        TableExpect.code(name)
    end

    # [ok, problem, message] for the model summary.
    def self.match(check, summary, file)
        errors = summary['errors'] || []
        unless errors.empty?
            first = errors.first(3).map { |p| "- Zeile #{p['line']}: #{p['msg']}" }.join("\n")
            more = errors.size > 3 ? "\n- … und #{errors.size - 3} weitere" : ''
            return [false, :errors, "In #{code(file)} #{errors.size == 1 ? 'steht noch ein Fehler' : "stehen noch #{errors.size} Fehler"}. Die ER-Werkstatt unterstreicht sie auch im Editor.\n\n#{first}#{more}"]
        end

        entities = (summary['entities'] || []).to_h { |e| [e['name'], e] }
        (check['entities'] || []).each do |spec|
            spec = { 'name' => spec } if spec.is_a?(String)
            name = spec['name']
            entity = entities[name]
            if entity.nil?
                other = entities.keys.find { |n| n.downcase == name.downcase }
                hint = other ? " Es gibt #{code(other)}; Namen schreibst du klein." : ''
                return [false, :missing_entity, "Es fehlt die Entität #{code(name)}.#{hint}"]
            end
            attrs = entity['attributes'].to_h { |a| [a['name'], a] }
            keys = entity['attributes'].select { |a| a['key'] }.map { |a| a['name'] }
            if spec['key'] && keys != spec['key']
                if keys.empty?
                    return [false, :key, "#{code(name)} hat noch keinen Schlüssel. Markiere #{spec['key'].size == 1 ? 'das Attribut' : 'die Attribute'} #{TableExpect.list(spec['key'])} mit `primary key`, zum Beispiel `#{spec['key'].first}: int, primary key`."]
                end
                return [false, :key, "Der Schlüssel von #{code(name)} ist #{TableExpect.list(keys)}, verlangt ist #{TableExpect.list(spec['key'])}."]
            end
            (spec['attributes'] || []).each do |text|
                want = parse_attribute(text)
                have = attrs[want[:name]]
                if have.nil?
                    return [false, :missing_attribute, "#{code(name)} hat noch kein Attribut #{code(want[:name])}. Es hat: #{TableExpect.list(attrs.keys)}."]
                end
                if want[:type] && have['type'].to_s.gsub(/\s+/, '').downcase != want[:type]
                    return [false, :attribute, "#{code("#{name}.#{want[:name]}")} hat den Typ `#{have['type']}`, verlangt ist `#{want[:type]}`."]
                end
                want[:flags].each do |flag|
                    next if have[flag]

                    what = { 'optional' => 'darf leer bleiben (`optional`)', 'unique' => 'muss eindeutig sein (`unique`)', 'key' => 'gehört zum Primärschlüssel (`primary key`)' }[flag]
                    return [false, :attribute, "#{code("#{name}.#{want[:name]}")} #{what}; das steht noch nicht im Modell."]
                end
            end
        end

        relations = summary['relationships'] || []
        (check['relationships'] || []).each do |spec|
            between = spec['between']
            hits = relations.select { |r| r['ends'].sort == between.sort }
            hits = hits.select { |r| r['name'] == spec['name'] } if spec['name'] && hits.any? { |r| r['name'] == spec['name'] }
            if hits.empty?
                return [false, :missing_relationship, "Es fehlt eine Beziehung zwischen #{TableExpect.list(between)}."]
            end
            r = hits.first
            if spec['cardinality']
                have = oriented(r, between)
                want = spec['cardinality'].tr('m', 'n')
                unless hits.any? { |h| oriented(h, between) == want }
                    return [false, :cardinality, "Die Beziehung #{code(r['name'])} zwischen #{code(between[0])} und #{code(between[1])} ist `#{have.sub(/n:n\z/, 'n:m')}`, gefragt ist `#{spec['cardinality']}`."]
                end
            end
            if spec['table'] && r['table'] != spec['table']
                return [false, :missing_table, "Die Beziehung #{code(r['name'])} wird zur Tabelle #{code(r['table'] || r['name'])}, verlangt ist #{code(spec['table'])}. Gib den Namen mit `as #{spec['table']}` an."]
            end
        end

        if check['tables']
            problem, message = TableExpect.compare(check['tables'], tables(summary), :model)
            return [false, problem, message] if problem
        end
        [true, nil, nil]
    end

    # Cardinality of a binary relationship read in the order of between,
    # with m written as n: »1:n«, »n:1«, »n:n«, »1:1«.
    def self.oriented(relation, between)
        cards = relation['cardinality'].map { |c| c.to_s.downcase.tr('m', 'n') }
        cards = cards.reverse if relation['ends'] != between && relation['ends'].reverse == between
        cards.join(':')
    end

    # The summary's tables in the TableExpect shape.
    def self.tables(summary)
        (summary['tables'] || []).to_h do |t|
            [t['name'], {
                :columns => t['columns'].map { |c| [c['name'], c['type']] },
                :pk => t['pk'] || [],
                :fks => (t['fks'] || []).map { |f| { :cols => f['cols'], :ref_table => f['refTable'], :ref_cols => f['refCols'] } },
                :rows => nil,
            }]
        end
    end

    def self.relation_text(r)
        if r['ends'].size == 2 && r['cardinality'].is_a?(Array)
            "#{r['name']}: #{r['ends'][0]} #{r['cardinality'].join(':')} #{r['ends'][1]}"
        else
            "#{r['name']}: #{r['ends'].join(', ')}"
        end
    end

    # Markdown: what the model really contains.
    def self.describe(summary, file)
        return "#{code(file)} gibt es nicht." if summary['missing']

        out = ["Gelesen: #{code(file)}"]
        entities = summary['entities'] || []
        unless entities.empty?
            out << ''
            out << '**Entitäten**'
            out << ''
            entities.each do |e|
                attrs = e['attributes'].map { |a| a['key'] ? "**#{a['name']}**" : a['name'] }
                out << "- #{code(e['name'])}: #{attrs.empty? ? 'keine Attribute' : attrs.join(', ')}"
            end
        end
        relations = summary['relationships'] || []
        unless relations.empty?
            out << ''
            out << '**Beziehungen**'
            out << ''
            relations.each { |r| out << "- `#{relation_text(r)}`#{r['table'] ? " → Tabelle #{code(r['table'])}" : ''}" }
        end
        tables = tables(summary)
        unless tables.empty?
            out << ''
            out << '**Daraus werden diese Tabellen** (Primärschlüssel fett)'
            out << ''
            out << TableExpect.describe(tables)
        end
        out << '' << 'Das Modell ist noch leer.' if entities.empty? && relations.empty?
        out.join("\n")
    end

    # [status, problem, message, read] (TaskCheck turns it into a Result).
    def self.evaluate(task, check, summary)
        file = check['file']
        return [:unavailable, nil, 'Die ER-Werkstatt hat das Modell nicht gelesen. Versuch es gleich noch einmal.', nil] if summary.nil?

        read = describe(summary, file)
        return [:wrong, :missing, "#{code(file)} gibt es nicht.", read] if summary['missing']

        ok, problem, message = match(check, summary, file)
        if ok
            return [:correct, nil, 'Passt.', read]
        end

        say = (task['feedback'] || []).find { |entry| entry['when']['problem'].to_s == problem.to_s }
        [:wrong, problem, say ? say['say'] : message, read]
    end
end
