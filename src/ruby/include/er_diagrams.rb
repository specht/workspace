require 'cgi'
require 'digest'
require 'json'
require 'open3'

# _include_er(film-1.er, chen) on a line of its own in a page: the diagram of
# the ER model film-1.er next to the page, drawn by the ER-Werkstatt core
# (src/vscode-extensions/er/core/er-core.js, the same file the extension and
# the er checks use) with node, as an SVG in the page. Its colours are the
# site's (--er-* in dark.css for the dark theme), its aria-label says what the
# model holds. Kinds: chen, crowfoot (the model), tables (after the
# translation). Positions kept by the extension in .film-1.er.layout.json
# next to the model are used; without them the core lays it out itself.
#
# A page and its model change together, so a diagram is never older than
# the model it shows (the content cache keys on the core and every content
# file).
module ErDiagrams
    CORE = File.expand_path('../../vscode-extensions/er/core/er-core.js', __dir__)
    LINE = /^_include_er\(\s*([^,()\s]+?)\s*,\s*([a-z]+)\s*\)[ \t]*$/
    KINDS = %w[chen crowfoot tables].freeze
    NAMES = { 'chen' => 'Chen-Diagramm', 'crowfoot' => 'Krähenfuß-Diagramm', 'tables' => 'Tabellendiagramm' }.freeze
    # argv: core, kind, positions as JSON; the model on stdin
    JS = <<~JS.gsub(/\s*\n\s*/, ' ')
        const core = require(process.argv[1]);
        let text = '';
        process.stdin.on('data', (d) => { text += d; });
        process.stdin.on('end', () => {
            const { model, result } = core.compile(text);
            if (model.errors.length) { process.stdout.write(JSON.stringify({ errors: model.errors })); return; }
            let positions = null;
            try { positions = JSON.parse(process.argv[3]); } catch (e) {}
            const d = core.diagram(text, process.argv[2], { positions });
            process.stdout.write(JSON.stringify({ svg: d.svg, summary: core.summary(model, result) }));
        });
    JS
    @cache = {}

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # { svg:, summary: } or { errors: } or { failure: message }
    def self.draw(text, kind, positions)
        key = Digest::SHA256.hexdigest([File.file?(CORE) ? File.binread(CORE) : '', text, kind, positions.to_json].join("\0"))
        @cache[key] ||= begin
            out, err, status = Open3.capture3('timeout', '20', 'node', '-e', JS, CORE, kind, positions.to_json, :stdin_data => text)
            status.success? ? JSON.parse(out, :symbolize_names => true) : { :failure => err.strip.lines.last.to_s.strip }
        rescue Errno::ENOENT => e
            { :failure => e.message }
        end
    end

    # What a reader without the picture needs: the entities or tables with
    # their columns, the relationships with their cardinality
    def self.alt(summary, kind)
        if kind == 'tables'
            tables = (summary[:tables] || []).map do |t|
                fks = (t[:fks] || []).flat_map { |f| f[:cols].map { |c| [c, f[:refTable]] } }.to_h
                cols = t[:columns].map do |c|
                    marks = []
                    marks << 'Schlüssel' if (t[:pk] || []).include?(c[:name])
                    marks << "Fremdschlüssel auf #{fks[c[:name]]}" if fks[c[:name]]
                    marks.empty? ? c[:name] : "#{c[:name]} (#{marks.join(', ')})"
                end
                "#{t[:name]} mit #{cols.join(', ')}"
            end
            return "#{NAMES[kind]}. Tabellen: #{tables.join('; ')}."
        end
        entities = (summary[:entities] || []).map do |e|
            "#{e[:name]} mit #{e[:attributes].map { |a| a[:key] ? "#{a[:name]} (Schlüssel)" : a[:name] }.join(', ')}"
        end
        relations = (summary[:relationships] || []).map do |r|
            how = if r[:participation]
                r[:ends].zip(r[:participation]).map { |e, (min, max)| "#{e} (#{min},#{max})" }.join(' und ')
            elsif r[:ends].size == 2
                "#{r[:ends][0]} #{r[:cardinality].join(':')} #{r[:ends][1]}"
            else
                r[:ends].join(', ')
            end
            "#{r[:name]} zwischen #{how}"
        end
        text = "#{NAMES[kind]}. Entitäten: #{entities.join('; ')}."
        text += " Beziehungen: #{relations.join('; ')}." unless relations.empty?
        text
    end

    def self.html(model_path, kind, warn: nil)
        label = "_include_er(#{File.basename(model_path)}, #{kind})"
        unless KINDS.include?(kind)
            warn&.call("#{label}: no such kind, take #{KINDS.join(', ')}")
            return nil
        end
        unless File.file?(model_path)
            warn&.call("#{label}: no such file")
            return nil
        end
        layout_path = File.join(File.dirname(model_path), ".#{File.basename(model_path)}.layout.json")
        positions = begin
            File.file?(layout_path) ? JSON.parse(File.read(layout_path))[kind] : nil
        rescue JSON::ParserError
            nil
        end
        drawn = draw(File.read(model_path, :encoding => 'UTF-8'), kind, positions)
        if drawn[:failure]
            warn&.call("#{label}: not drawn (#{drawn[:failure]})")
            return nil
        end
        if drawn[:errors]
            first = drawn[:errors].first
            warn&.call("#{label}: the model has #{drawn[:errors].size} error(s), line #{first[:line]}: #{first[:msg]}")
            return nil
        end
        svg = drawn[:svg].sub(/aria-label="[^"]*"/) { "aria-label=\"#{h(alt(drawn[:summary], kind))}\"" }
        "<div class='er-figure'>#{svg}</div>"
    end

    def self.expand(markdown, page_dir:, warn: nil)
        markdown.gsub(LINE) do
            whole = $&
            html(File.join(page_dir, $1), $2, :warn => warn) || whole
        end
    end
end
