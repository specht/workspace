require 'cgi'
require 'digest'
require 'json'
require 'open3'

# _uml_diagram on a line of its own, directly under a ```uml block: the
# class diagram of that block, drawn by the UML-Werkstatt core
# (src/vscode-extensions/uml/core/uml-core.js, the same file the extension,
# the playground in the Handbuch and the classes checks use) with node, as an
# SVG in the page under the text it is drawn from. Its colours are the
# site's (--cd-* in dark.css for the dark theme), its aria-label says what
# the diagram holds. The block stays as it is, so the reader sees text and
# diagram together.
#
#   ```uml
#   class Vehicle
#     - wheels: int
#   ```
#
#   _uml_diagram
#
# Like ErDiagrams, a diagram is never older than its text: the content cache
# keys on the core and every content file.
module UmlDiagrams
    CORE = File.expand_path('../../vscode-extensions/uml/core/uml-core.js', __dir__)
    # the lines of one block only: none of them starts a fence
    BLOCK = /^```uml[ \t]*\n((?:(?!```)[^\n]*\n)*)```[ \t]*\n(?:[ \t]*\n)*_uml_diagram[ \t]*$/
    # argv: core; the text on stdin
    JS = <<~JS.gsub(/\s*\n\s*/, ' ')
        const core = require(process.argv[1]);
        let text = '';
        process.stdin.on('data', (d) => { text += d; });
        process.stdin.on('end', () => {
            const model = core.parse(text);
            if (model.errors.length) { process.stdout.write(JSON.stringify({ errors: core.summary(model).errors })); return; }
            const d = core.diagram(text, { background: null });
            process.stdout.write(JSON.stringify({ svg: d.svg, summary: core.summary(model) }));
        });
    JS
    @cache = {}

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # { svg:, summary: } or { errors: } or { failure: message }
    def self.draw(text)
        # bytes on both sides: the core is read as BINARY, the text is UTF-8
        key = Digest::SHA256.hexdigest([File.file?(CORE) ? File.binread(CORE) : '', text.b].join("\0".b))
        @cache[key] ||= begin
            out, err, status = Open3.capture3('timeout', '20', 'node', '-e', JS, CORE, :stdin_data => text)
            status.success? ? JSON.parse(out.force_encoding('UTF-8'), :symbolize_names => true) : { :failure => err.strip.lines.last.to_s.strip }
        rescue Errno::ENOENT => e
            { :failure => e.message }
        end
    end

    # What a reader without the picture needs: the classes with what they
    # hold, who inherits from whom, the associations with their multiplicities
    def self.alt(summary)
        classes = (summary[:classes] || []).map do |c|
            members = c[:attributes].map { |a| "#{a[:name]}: #{a[:type]}" } +
                c[:methods].map { |m| "#{m[:name]}(#{m[:params].map { |p| p[:name] }.join(', ')})" }
            name = c[:abstract] ? "#{c[:name]} (abstrakt)" : c[:name]
            name += ", erbt von #{c[:extends]}" if c[:extends]
            members.empty? ? name : "#{name} mit #{members.join(', ')}"
        end
        text = "Klassendiagramm. Klassen: #{classes.join('; ')}."
        associations = (summary[:associations] || []).map do |a|
            "#{a[:name]} zwischen #{a[:from]} (#{a[:fromMult]}) und #{a[:to]} (#{a[:toMult]})"
        end
        text += " Assoziationen: #{associations.join('; ')}." unless associations.empty?
        text
    end

    def self.html(text, warn: nil)
        drawn = draw(text)
        if drawn[:failure]
            warn&.call("_uml_diagram: not drawn (#{drawn[:failure]})")
            return nil
        end
        if drawn[:errors]
            first = drawn[:errors].first
            warn&.call("_uml_diagram: the diagram has #{drawn[:errors].size} error(s), line #{first[:line]}: #{first[:msg]}")
            return nil
        end
        svg = drawn[:svg].sub(/aria-label="[^"]*"/) { "aria-label=\"#{h(alt(drawn[:summary]))}\"" }
        # no empty line inside, or Markdown would end the HTML block there
        "<div class='uml-figure'>#{svg.gsub(/\n\s*\n/, "\n")}</div>"
    end

    def self.expand(markdown, warn: nil)
        markdown.gsub(BLOCK) do
            whole = $&
            text = $1
            figure = html(text, :warn => warn)
            figure ? "```uml\n#{text}```\n\n#{figure}" : whole
        end
    end
end
