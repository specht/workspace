require 'json'
require 'shellwords'
require_relative 'scene_color'

# Check type scene (docs/aufgaben.md): what is where in an Anaglyph scene.
#
# The server never evaluates a scene. It runs scene_dump.js in the student's
# Workspace with the student's own viewer (anaglyph/include/script.js) and
# compares the JSON it prints with the task's expectations. Everything here
# works on that JSON and can be tested without a Workspace.
module SceneCheck
    DUMP_JS = File.read(File.join(__dir__, 'scene_dump.js'), encoding: 'UTF-8')

    # p5 sizes of the built-in shapes before scaling (width, height, depth).
    BASE_SIZE = {
        'box' => [50, 50, 50], 'sphere' => [100, 100, 100], 'cylinder' => [100, 100, 100],
        'cone' => [100, 100, 100], 'torus' => [140, 140, 40], 'plane' => [100, 100, 0],
        'grid' => [1000, 0, 1000],
    }.freeze
    NAMES = {
        'box' => %w[Würfel Würfel], 'sphere' => %w[Kugel Kugeln], 'cylinder' => %w[Zylinder Zylinder],
        'cone' => %w[Kegel Kegel], 'torus' => %w[Ring Ringe], 'teapot' => %w[Teekanne Teekannen],
        'plane' => %w[Fläche Flächen], 'grid' => %w[Gitter Gitter],
    }.freeze
    TOLERANCE = 2.0

    # The command that prints the dump, run in /workspace as the student.
    def self.command(file, times)
        viewer = File.join(File.dirname(file), 'include', 'script.js')
        Shellwords.join(['node', '-e', DUMP_JS, viewer, file, times.map { |t| t.to_s }.join(',')])
    end

    def self.parse(output)
        JSON.parse(output.to_s)
    rescue JSON::ParserError
        nil
    end

    def self.shape_name(shape, n = 1)
        names = NAMES[shape.to_s]
        return "#{n == 1 ? 'Objekt' : 'Objekte'} »#{shape}«" unless names
        n == 1 ? names[0] : names[1]
    end

    def self.fmt(number)
        r = number.to_f.round(1)
        (r == r.round ? r.round.to_s : r.to_s.sub('.', ','))
    end

    def self.pos_text(pos)
        "(#{pos.map { |v| fmt(v) }.join(', ')})"
    end

    def self.size_of(obj, at = obj['at'].first)
        base = BASE_SIZE[obj['shape'].to_s]
        return nil unless base

        base.each_with_index.map { |b, i| b * at['scale'][i].to_f }
    end

    def self.near?(a, b, tol = TOLERANCE)
        a.zip(b).all? { |x, y| (x.to_f - y.to_f).abs <= tol }
    end

    def self.describe(dump, ignore)
        objects = dump['objects'].reject { |o| ignore.include?(o['shape']) }
        lines = objects.first(15).map do |o|
            what = o['shape'] ? shape_name(o['shape']) : (o['model'] ? "Modell #{o['model']}" : "Text »#{o['text']}«")
            name = o['name'] ? " »#{o['name']}«" : ''
            size = size_of(o)
            "- Zeile #{o['line']}: #{what}#{name} bei #{pos_text(o['at'].first['pos'])}" +
                (size ? ", Größe #{size.map { |v| fmt(v) }.join(' × ')}" : '') +
                (o.key?('fill') ? ", Farbe #{SceneColor.describe(o['fill'])}" : '')
        end
        more = objects.size > 15 ? "\n- … und #{objects.size - 15} weitere" : ''
        background = dump['background'] ? "\n\nHintergrund: #{SceneColor.describe(dump['background'])}" : ''
        "#{objects.size} #{objects.size == 1 ? 'Objekt' : 'Objekte'} in der Szene:\n\n#{lines.join("\n")}#{more}#{background}"
    end

    def self.color_family(obj)
        SceneColor.family(SceneColor.rgb(obj['fill']))
    end

    # The objects an expectation is about: its shape, name and colour.
    def self.candidates(m, objects)
        families = m['color'] && Array(m['color'])
        objects.select do |o|
            (m['shape'].nil? || o['shape'] == m['shape']) &&
                (m['name'].nil? || o['name'] == m['name']) &&
                (families.nil? || families.include?(color_family(o)))
        end
    end

    # Accusative articles: »ich finde einen Würfel / eine Kugel / ein Gitter«.
    ARTICLES = {
        :m => %w[einen keinen], :f => %w[eine keine], :n => %w[ein kein],
    }.freeze
    GENDER = {
        'box' => :m, 'cylinder' => :m, 'cone' => :m, 'torus' => :m,
        'sphere' => :f, 'teapot' => :f, 'plane' => :f, 'grid' => :n,
    }.freeze

    # »eine Kugel in Gelb oder Orange«; negated: »keine Kugel in Gelb«
    def self.what_text(m, negated: false)
        article = ARTICLES[GENDER[m['shape'].to_s] || :n][negated ? 1 : 0]
        thing = m['shape'] && NAMES[m['shape'].to_s] ? "#{article} #{shape_name(m['shape'])}" : "#{negated ? 'kein' : 'ein'} Objekt"
        colour = m['color'] ? " in #{SceneColor.german_list(m['color'])}" : ''
        name = m['name'] ? " mit `name = #{m['name']}`" : ''
        "#{thing}#{colour}#{name}"
    end

    # Returns [ok, problem, message] for one expectation.
    # matchers: all expectations of the check, so orbits can name another
    # one by its label.
    def self.match(m, objects, all, matchers = [])
        shape = m['shape']
        candidates = candidates(m, objects)
        label = m['label'] ? "»#{m['label']}«" : (m['name'] ? "»#{m['name']}«" : shape_name(shape, 2))

        if (m['name'] || m['color']) && candidates.empty?
            for_label = m['label'] ? " für #{label}" : ''
            others = m['color'] ? objects.select { |o| shape.nil? || o['shape'] == shape } : []
            seen = others.empty? ? '' : " Ich sehe #{others.first(4).map { |o| "#{shape_name(o['shape'])} in #{SceneColor::GERMAN[color_family(o)] || 'unbekannter Farbe'} (Zeile #{o['line']})" }.join(', ')}."
            return [false, :missing_object, "Ich finde #{what_text(m, negated: true)}#{for_label}.#{seen}"]
        end
        if m.key?('count') && candidates.size != m['count']
            return [false, :count, "Ich finde #{candidates.size} #{shape_name(shape, candidates.size)}, verlangt #{m['count'] == 1 ? "ist #{shape == 'box' ? 'einer' : 'eins'}" : "sind #{m['count']}"}."]
        end
        if m.key?('min') && candidates.size < m['min']
            return [false, :count, "Ich finde #{candidates.size} #{shape_name(shape, candidates.size)}, verlangt sind mindestens #{m['min']}."]
        end
        if candidates.empty?
            return [false, :count, "Ich finde keine #{shape_name(shape, 2)}."]
        end

        first = ->(o) { o['at'].first }
        if m['at']
            if m.key?('count')
                off = candidates.reject { |o| near?(first.(o)['pos'], m['at']) }
                unless off.empty?
                    return [false, :position, "#{label.capitalize} #{candidates.size == 1 ? 'soll' : 'sollen alle'} bei #{pos_text(m['at'])} stehen, aber #{off.map { |o| "der aus Zeile #{o['line']} steht bei #{pos_text(first.(o)['pos'])}" }.first(2).join(', ')}."]
                end
            elsif candidates.none? { |o| near?(first.(o)['pos'], m['at']) }
                return [false, :position, "Es fehlt #{shape == 'box' ? 'ein' : 'eine'} #{shape_name(shape)} bei #{pos_text(m['at'])}."]
            end
        end
        if m['size']
            sized = candidates.map { |o| [o, size_of(o)] }
            bad = sized.reject { |_, s| s && s.zip(m['size']).all? { |a, b| (a - b).abs <= [TOLERANCE, b.abs * 0.02].max } }
            unless bad.empty?
                o, s = bad.first
                return [false, :size, "#{shape_name(shape).capitalize} aus Zeile #{o['line']} ist #{s ? s.map { |v| fmt(v) }.join(' × ') : 'unbekannt'} groß, verlangt #{m['size'].map { |v| fmt(v) }.join(' × ')}."]
            end
        end
        if m.key?('bottom_y')
            candidates.each do |o|
                s = size_of(o)
                next unless s

                bottom = first.(o)['pos'][1] - s[1] / 2.0
                next if (bottom - m['bottom_y']).abs <= TOLERANCE

                hint = bottom < m['bottom_y'] ? ' Sie versinkt im Gitter.' : ' Sie schwebt.'
                return [false, :bottom, "Die Unterkante von #{shape_name(shape)} aus Zeile #{o['line']} liegt bei y = #{fmt(bottom)}, verlangt bei y = #{fmt(m['bottom_y'])}.#{hint}"]
            end
        end
        if (rz = m['rotate_z'])
            modulo = rz['modulo'] || 360
            angles = candidates.map { |o| (first.(o)['rot_z'].to_f % modulo).round(1) % modulo }.sort
            diffs = angles.each_cons(2).map { |a, b| b - a } + [angles.first + modulo - angles.last]
            unless angles.uniq.size == angles.size && diffs.all? { |d| (d - rz['spacing']).abs <= 1 }
                return [false, :rotation, "Die #{shape_name(shape, 2)} sind um #{candidates.map { |o| "#{fmt(first.(o)['rot_z'])}°" }.join(', ')} um die z-Achse gedreht. Verlangt sind gleichmäßige Abstände von #{rz['spacing']}°."]
            end
        end
        if (lattice = m['lattice'])
            positions = candidates.map { |o| first.(o)['pos'].map { |v| v.round } }
            values = (0..2).map { |i| positions.map { |p| p[i] }.uniq.sort }
            complete = values.map(&:size) == lattice && positions.uniq.size == lattice.reduce(:*) &&
                values[0].product(values[1], values[2]).all? { |p| positions.include?(p) }
            unless complete
                return [false, :lattice, "Die #{candidates.size} #{shape_name(shape, candidates.size)} bilden kein #{lattice.join(' × ')}-Gitter: Ich finde #{values[0].size} verschiedene x-Werte, #{values[1].size} y-Werte und #{values[2].size} z-Werte."]
            end
            if m['min_spacing']
                gaps = values.flat_map { |v| v.each_cons(2).map { |a, b| b - a } }
                if gaps.any? { |g| g < m['min_spacing'] - TOLERANCE }
                    return [false, :spacing, "Die #{shape_name(shape, 2)} stehen zu dicht: Der kleinste Abstand ist #{fmt(gaps.min)}, verlangt sind mindestens #{m['min_spacing']}."]
                end
            end
        end
        if (rows = m['rows'])
            positions = candidates.map { |o| first.(o)['pos'].map(&:round) }
            same = positions.group_by(&:itself).find { |_, list| list.size > 1 }
            if same
                return [false, :rows, "#{same[1].size} #{shape_name(shape, same[1].size)} stehen an derselben Stelle, bei #{pos_text(same[0])}."]
            end
            even = [0, 2].any? do |i|
                groups = positions.group_by { |p| p[i] }.values
                groups.size == rows && groups.map(&:size).uniq.size == 1
            end
            unless even
                return [false, :rows, "Die #{shape_name(shape, 2)} stehen nicht in #{rows} gleich langen Reihen."]
            end
        end
        if m.key?('moves')
            moving = candidates.select { |o| o['at'].map { |a| a['pos'] }.uniq { |p| p.map { |v| v.round } }.size > 1 }
            if m['moves'] == false && !moving.empty?
                return [false, :moves, "#{label.capitalize} bewegt sich, soll aber stillstehen."]
            elsif m['moves'] == true && moving.empty?
                return [false, :moves, "#{label.capitalize} bewegt sich nicht."]
            end
        end
        if (center_name = m['orbits'])
            # orbits names another expectation by its label, or an object by name.
            other = matchers.find { |x| x['label'] == center_name }
            if other
                centers = candidates(other, objects)
                return [false, :missing_object, "Ich finde #{what_text(other)} für »#{center_name}« nicht."] if centers.empty?
                if centers.size > 1
                    return [false, :ambiguous, "Ich finde #{centers.size} Objekte, die »#{center_name}« sein könnten (Zeilen #{centers.map { |o| o['line'] }.join(', ')}). Gib ihnen verschiedene Farben."]
                end
                center = centers.first
            else
                center = all.find { |o| o['name'] == center_name }
                return [false, :missing_object, "Ich finde kein Objekt mit `name = #{center_name}`."] unless center
            end

            candidates.each do |o|
                distances = o['at'].each_index.map do |i|
                    a = o['at'][i]['pos']
                    b = center['at'][i]['pos']
                    Math.sqrt(a.zip(b).sum { |x, y| (x - y)**2 })
                end
                moves = o['at'].map { |a| a['pos'].map(&:round) }.uniq.size > 1
                steady = distances.max - distances.min <= [TOLERANCE, distances.max * 0.03].max
                unless moves && steady && distances.min > TOLERANCE
                    why = !moves ? 'bewegt sich nicht' : (distances.min <= TOLERANCE ? 'steht mitten in ihm' : "ihr Abstand ändert sich (#{distances.map { |d| fmt(d) }.join(', ')})")
                    return [false, :orbit, "#{label.capitalize} kreist nicht um »#{center_name}«: #{label} #{why}."]
                end
            end
        end
        [true, nil, nil]
    end

    # task, check from tasks.yaml; dump = parsed JSON; source = the .ini text.
    # Returns [status, problem, message, read].
    def self.evaluate(task, check, dump, source)
        file = check['file']
        if dump.nil?
            return [:unavailable, :dump, 'Die Szene konnte gerade nicht gelesen werden. Versuch es gleich noch einmal.', nil]
        end
        if dump['missing']
            return [:wrong, :missing, "`#{file}` gibt es nicht. Lege die Datei im Ordner `#{File.dirname(file)}` an.", nil]
        end
        ignore = check['ignore'] || []
        read = describe(dump, ignore)
        errors = dump['errors'] || []
        unless errors.empty?
            shown = errors.first(3).map { |e| "- #{e}" }.join("\n")
            return [:wrong, :scene_error, "Der Viewer meldet Fehler in `#{file}`:\n\n#{shown}", read]
        end

        objects = dump['objects'].reject { |o| ignore.include?(o['shape']) }
        (check['source'] || {}).fetch('contains', []).each do |word|
            next if source.to_s =~ /(^|\s)#{Regexp.escape(word)}\b/

            return feedback(task, objects, source, [:wrong, :source, "Benutze in `#{file}` `#{word}`.", read])
        end
        if (wanted = check['background'])
            family = SceneColor.family(SceneColor.rgb(dump['background']))
            unless Array(wanted).include?(family)
                message = "Der Hintergrund ist #{SceneColor.describe(dump['background'])}, verlangt ist #{SceneColor.german_list(wanted)}. `background = …` ändert ihn."
                return feedback(task, objects, source, [:wrong, :background, message, read])
            end
        end
        matchers = check['expect'].is_a?(Hash) ? [check['expect']] : check['expect']
        matchers.each do |m|
            ok, problem, message = match(m, objects, dump['objects'], matchers)
            next if ok

            return feedback(task, objects, source, [:wrong, problem, message, read])
        end
        [:correct, nil, 'Passt.', read]
    end

    # Feedback rules: found/missing positions, source_matches and problem.
    def self.feedback(task, objects, source, result)
        (task['feedback'] || []).each do |entry|
            w = entry['when']
            ok = true
            ok &&= objects.any? { |o| near?(o['at'].first['pos'], w['found']) } if w['found']
            ok &&= objects.none? { |o| near?(o['at'].first['pos'], w['missing']) } if w['missing']
            if w['source_matches']
                ok &&= begin
                    Regexp.new(w['source_matches'], timeout: 0.5).match?(source.to_s)
                rescue RegexpError, Regexp::TimeoutError
                    false
                end
            end
            ok &&= result[1].to_s == w['problem'].to_s if w['problem']
            next unless ok && (w['found'] || w['missing'] || w['source_matches'] || w['problem'])

            return [result[0], result[1], entry['say'], result[3]]
        end
        result
    end
end
