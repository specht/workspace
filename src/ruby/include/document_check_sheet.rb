# The rules for a spreadsheet (docs/aufgaben.md, »document«, Tabellen;
# docs/plan-tabellenkalkulation.md). The office service reads the workbook
# (src/office/sheet.py): each sheet's cells with value, formula, the text
# as shown and the number format, its columns and rows, filters,
# conditional formats and charts, and draws each sheet as a grid, where the
# marks of these rules stand on the cells. A rule names cells as the
# programs do (»B3«, »D2:D9«), on the first sheet unless it says sheet:.
#
# recalc: the service changes input cells, recalculates and reads others
# (probes, sent with the upload): the check that a table really calculates,
# as the checks of code run it.
module DocumentCheck
    SHEET_RULES = %w[formulas values recalc functions absolute same_data number_format text_numbers bold_cells
                     sorted autofilter conditional chart no_errors no_constants ranges].freeze

    # German names of the functions a class meets (LibreOffice and Excel
    # show them so; the file stores the English ones)
    FUNCTION_NAMES = { 'SUM' => 'SUMME', 'AVERAGE' => 'MITTELWERT', 'MAX' => 'MAX', 'MIN' => 'MIN', 'COUNT' => 'ANZAHL',
                       'COUNTA' => 'ANZAHL2', 'IF' => 'WENN', 'COUNTIF' => 'ZÄHLENWENN', 'SUMIF' => 'SUMMEWENN',
                       'ROUND' => 'RUNDEN', 'ROUNDDOWN' => 'ABRUNDEN', 'ROUNDUP' => 'AUFRUNDEN', 'SUBTOTAL' => 'TEILERGEBNIS',
                       'TODAY' => 'HEUTE' }.freeze
    CHART_NAMES = { 'column' => 'Säulendiagramm', 'bar' => 'Balkendiagramm', 'line' => 'Liniendiagramm',
                    'pie' => 'Kreisdiagramm', 'scatter' => 'Punktdiagramm', 'area' => 'Flächendiagramm' }.freeze
    # com.sun.star.util.NumberFormat
    FORMAT_KINDS = { 'date' => 2, 'currency' => 8, 'percent' => 128 }.freeze
    FORMAT_NAMES = { 'date' => 'Datum', 'currency' => 'Währung', 'percent' => 'Prozent' }.freeze

    def self.sheet?(model)
        model.is_a?(Hash) && model['kind'] == 'sheet'
    end

    # A check on a spreadsheet: its starting file is one (.xlsx, .ods)
    def self.sheet_check?(check)
        check['file'].to_s.match?(/\.(xlsx|ods|xls)\z/i)
    end

    # The probes the service runs with the upload: one for each recalc
    # rule, in their order.
    def self.probes(check)
        Array(check['rules']).select { |r| r.is_a?(Hash) && r.key?('recalc') }.map do |r|
            { 'sheet' => r['sheet'].to_i, 'set' => r['recalc']['set'], 'read' => r['recalc']['expect'].keys }
        end
    end

    # ---- cells

    def self.cell_index(text)
        m = text.to_s.strip.upcase.delete('$').match(/\A([A-Z]{1,3})(\d+)\z/)
        raise ArgumentError, text.to_s unless m

        col = m[1].chars.reduce(0) { |n, ch| n * 26 + ch.ord - 64 } - 1
        [col, m[2].to_i - 1]
    end

    def self.cell_name(col, row)
        name = +''
        c = col + 1
        while c.positive?
            c, r = (c - 1).divmod(26)
            name.prepend((65 + r).chr)
        end
        "#{name}#{row + 1}"
    end

    # »D2:D9« or »D5« to the cells' names, row by row
    def self.cells_of(range)
        a, b = range.to_s.split(':')
        c1, r1 = cell_index(a)
        c2, r2 = b ? cell_index(b) : [c1, r1]
        (r1..r2).flat_map { |r| (c1..c2).map { |c| cell_name(c, r) } }
    end

    def self.sheet_of(model, rule)
        (model['sheets'] || [])[rule.is_a?(Hash) ? rule['sheet'].to_i : 0] || {}
    end

    # (the index kept in the sheet's own data, which lives for one check)
    def self.cell(sheet, name)
        (sheet['_index'] ||= (sheet['cells'] || []).to_h { |c| [c['cell'], c] })[name.to_s.delete('$').upcase]
    end

    # where a cell stands in the drawing of its sheet (page: the sheet's
    # number, from 1)
    def self.cell_mark(model, sheet_index, name, title)
        sheet = (model['sheets'] || [])[sheet_index] || {}
        col, row = cell_index(name)
        c = (sheet['cols'] || []).find { |x| x['col'] == col }
        r = (sheet['rows'] || []).find { |x| x['row'] == row }
        return {} if c.nil? || r.nil? || !r['visible']

        { :page => sheet_index + 1, :x => sheet['header_w'].to_f + c['x'], :y => sheet['header_h'].to_f + r['y'],
          :w => c['w'], :h => r['h'], :kind => 'cell', :title => title }
    end

    def self.shown(cell)
        cell ? (cell['shown'] || cell['text'] || cell['value']).to_s : ''
    end

    def self.number(value)
        format('%.2f', value.to_f).sub(/\.?0+\z/, '').tr('.', ',')
    end

    def self.close?(a, b)
        !a.nil? && !b.nil? && (a.to_f - b.to_f).abs <= [0.005, b.to_f.abs * 1e-9].max
    end

    # a cell as a rule wants it: a number close enough, or a text (»wieder«,
    # a result of WENN) as the cell shows it
    def self.matches?(value, shown, want)
        want.is_a?(String) ? shown.to_s.strip == want : close?(value, want)
    end

    # text as the programs sort it in German: Ç with C, ą with a, ß as ss
    def self.sort_key(text)
        text.to_s.unicode_normalize(:nfkd).gsub(/\p{Mn}/, '').gsub('ß', 'ss').downcase
    end

    def self.wanted(want)
        want.is_a?(String) ? q(want) : number(want)
    end

    def self.german_formula(formula)
        formula.to_s.gsub(/\b([A-Z]+)\(/) { "#{FUNCTION_NAMES[Regexp.last_match(1)] || Regexp.last_match(1)}(" }.tr(',', ';')
    end

    def self.cells_label(ranges)
        names(Array(ranges).map { |r| r.to_s })
    end

    # ---- the rules

    # these cells calculate: a formula, not a number someone worked out
    def self.rule_formulas(rule, _body, _starter, _all = nil, model = nil)
        ranges = Array(rule['formulas'])
        sheet = sheet_of(model, rule)
        item = Item.new(:rule => 'formulas', :label => "#{cells_label(ranges)} #{ranges.size == 1 && !ranges.first.include?(':') ? 'rechnet' : 'rechnen'} mit Formeln",
                        :ok => true, :marks => [])
        typed = ranges.flat_map { |r| cells_of(r) }.reject { |n| cell(sheet, n) && cell(sheet, n)['type'] == 'formula' }
        return item if typed.empty?

        item.ok = false
        item.marks = typed.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'keine Formel') }
        item.problem = typed.size == 1 ? "#{typed.first} ohne Formel" : "#{typed.size} Zellen ohne Formel"
        first = cell(sheet, typed.first)
        item.message = "#{typed.size == 1 ? "In #{typed.first} steht" : "In #{typed.size} Zellen, zum Beispiel #{typed.first}, steht"} " \
            "#{first ? "eine getippte #{first['type'] == 'text' ? 'Angabe' : 'Zahl'} (#{q(shown(first))})" : 'nichts'}, keine Formel. " \
            'Ändert sich eine Zahl, mit der sie gerechnet wurde, bleibt sie falsch. Eine Formel beginnt mit =, zum Beispiel =B2*C2.'
        item
    end

    # The numbers typed into a formula (»=29*148«, »=6595«): what is left
    # of it without strings, cell addresses and function names
    def self.constants(formula)
        rest = formula.to_s.sub(/\A=/, '').gsub(/"[^"]*"/, '')
        rest = rest.gsub(/(?:[\w.]+[.!])?\$?[A-Z]{1,3}\$?\d+(?::\$?[A-Z]{1,3}\$?\d+)?/i, 'R').gsub(/[A-Z][A-Z0-9.]*\(/i, '(')
        rest.scan(/\d+(?:[.,]\d+)?/)
    end

    # these cells calculate with cells, not with numbers typed into the
    # formula: =29*148 is right today and wrong when the price changes,
    # and the recalc of one cell finds it in that one only
    def self.rule_no_constants(rule, _body, _starter, _all = nil, model = nil)
        ranges = Array(rule['no_constants'])
        sheet = sheet_of(model, rule)
        one = ranges.size == 1 && !ranges.first.include?(':')
        item = Item.new(:rule => 'no_constants', :label => "#{cells_label(ranges)} #{one ? 'rechnet' : 'rechnen'} mit Zellen", :ok => true, :marks => [])
        found = ranges.flat_map { |r| cells_of(r) }.select do |n|
            c = cell(sheet, n)
            c && c['type'] == 'formula' && !constants(c['formula']).empty?
        end
        return item if found.empty?

        item.ok = false
        item.marks = found.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'getippte Zahlen') }
        item.problem = found.size == 1 ? "#{found.first} rechnet mit getippten Zahlen" : "#{found.size} Zellen rechnen mit getippten Zahlen"
        f = cell(sheet, found.first)['formula']
        item.message = "#{found.size == 1 ? "In #{found.first} steht" : "In #{found.size} Zellen, zum Beispiel #{found.first}, steht"} " \
            "»#{q(german_formula(f))}«: eine Formel mit Zahlen, die jemand ausgerechnet oder abgeschrieben hat. Heute stimmt das Ergebnis; " \
            'ändert sich eine dieser Zahlen in der Tabelle, rechnet die Zelle nicht mit. Schreib die Adressen der Zellen statt ihrer Zahlen, zum Beispiel =B2*C2.'
        item
    end

    # a function in these cells adds up (or averages) exactly this range:
    # =SUMME(D4:D7) misses the last row (written so, or a row inserted
    # right above the total and left out). A cell without any range in its
    # formula is the business of formulas and functions.
    def self.rule_ranges(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['ranges'] || {}
        sheet = sheet_of(model, rule)
        item = Item.new(:rule => 'ranges', :label => names(wants.map { |n, r| "#{n} über #{r.to_s.upcase}" }), :ok => true, :marks => [])
        wrong = wants.filter_map do |n, want|
            f = (cell(sheet, n) || {})['formula'].to_s.upcase.delete('$')
            have = f.scan(/(?<![A-Z])([A-Z]{1,3}\d+:[A-Z]{1,3}\d+)/).flatten
            next if have.empty? || have.include?(want.to_s.upcase.delete('$'))

            [n, want.to_s.upcase, have.first]
        end
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.map { |n, _, _| cell_mark(model, rule['sheet'].to_i, n, 'falscher Bereich') }
        n, want, have = wrong.first
        missing = cells_of(want) - cells_of(have)
        extra = cells_of(have) - cells_of(want)
        item.problem = missing.empty? ? "#{n} zählt zu viel mit" : "#{n} lässt #{missing.size == 1 ? missing.first : "#{missing.size} Zellen"} aus"
        item.message = "#{n} rechnet über #{have}, die Zahlen stehen aber in #{want}: " +
            (missing.empty? ? "#{names(extra.first(3))} #{extra.size == 1 ? 'gehört' : 'gehören'} nicht dazu. " : "#{names(missing.first(3))} #{missing.size == 1 ? 'zählt' : 'zählen'} nicht mit. ") +
            "Ändere den Bereich in der Formel auf #{want}."
        item
    end

    # these cells show these values
    def self.rule_values(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['values'] || {}
        sheet = sheet_of(model, rule)
        label = wants.size == 1 ? "Das Ergebnis in #{wants.keys.first} stimmt" : "Die Ergebnisse in #{cells_label(wants.keys)} stimmen"
        item = Item.new(:rule => 'values', :label => label, :ok => true, :marks => [])
        wrong = wants.reject { |n, v| c = cell(sheet, n) || {}; matches?(c['value'], shown(c), v) }
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'falsches Ergebnis') }
        n, v = wrong.first
        item.problem = wrong.size == 1 ? "Falsches Ergebnis in #{n}" : "#{wrong.size} falsche Ergebnisse"
        have = cell(sheet, n)
        item.message = "In #{n} steht #{have ? q(shown(have)) : 'nichts'}, richtig wäre #{wanted(v)}." +
            (have && have['formula'] ? " Die Formel dort: #{q(german_formula(have['formula']))}." : '')
        item
    end

    # change input cells, recalculate: the results follow
    def self.rule_recalc(rule, _body, _starter, _all = nil, model = nil)
        set = rule['recalc']['set']
        expect = rule['recalc']['expect']
        probe = (model['probes'] || [])[rule['_probe'].to_i] || {}
        changes = names(set.map { |n, v| "#{n} auf #{v.is_a?(Numeric) ? number(v) : v}" })
        item = Item.new(:rule => 'recalc', :label => 'Rechnet mit, wenn sich eine Zahl ändert', :ok => true, :marks => [])
        wrong = expect.reject { |n, v| matches?((probe['values'] || {})[n], (probe['shown'] || {})[n], v) }
        return item if wrong.empty? && probe['error'].nil?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'rechnet nicht mit') }
        n, v = wrong.first || expect.first
        got = (probe['shown'] || {})[n]
        item.problem = "#{n} rechnet nicht mit"
        item.message = "#{n} rechnet nicht mit: Nachdem ich #{changes} geändert habe, müsste dort #{wanted(v)} stehen, " \
            "es steht aber #{got.to_s.empty? ? 'nichts' : q(got)} da. Rechnet die Zelle mit einer getippten Zahl statt mit der Zelle?"
        item
    end

    # these cells use a function (SUMME, MITTELWERT …)
    def self.rule_functions(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['functions'] || {}
        sheet = sheet_of(model, rule)
        label = names(wants.map { |n, f| "#{n} mit #{FUNCTION_NAMES[f.to_s.upcase] || f}" })
        item = Item.new(:rule => 'functions', :label => label, :ok => true, :marks => [])
        # a key may be a range (»B33:E33«): each of its cells
        wrong = wants.flat_map { |n, f| cells_of(n).map { |c| [c, f] } }
                     .reject { |n, f| (cell(sheet, n) || {})['formula'].to_s.upcase.match?(/(?<![A-Z])#{f.to_s.upcase}\(/) }.to_h
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'ohne die Funktion') }
        n, f = wrong.first
        have = cell(sheet, n)
        name = FUNCTION_NAMES[f.to_s.upcase] || f
        item.problem = wrong.size == 1 ? "#{n} ohne #{name}" : "#{wrong.size} Zellen ohne #{name}"
        item.message = "#{n} rechnet noch nicht mit #{name}#{have && have['formula'] ? ", dort steht #{q(german_formula(have['formula']))}" : ''}. " \
            "Eine Funktion wie =#{name}(B2:B9) rechnet einen ganzen Bereich auf einmal und bleibt richtig, wenn eine Zeile dazukommt."
        item
    end

    # copied formulas keep one cell fixed: $ before its row (and column)
    def self.rule_absolute(rule, _body, _starter, _all = nil, model = nil)
        ref = rule['absolute']['ref'].to_s.delete('$').upcase
        col, row = ref.match(/\A([A-Z]+)(\d+)\z/).captures
        sheet = sheet_of(model, rule)
        item = Item.new(:rule => 'absolute', :label => "Fester Bezug auf #{ref}", :ok => true, :marks => [])
        loose = cells_of(rule['absolute']['cells']).reject do |n|
            f = (cell(sheet, n) || {})['formula'].to_s.upcase
            f.match?(/\$?#{col}\$#{row}(?!\d)/)
        end
        return item if loose.empty?

        item.ok = false
        item.marks = loose.map { |n| cell_mark(model, rule['sheet'].to_i, n, "ohne $#{col}$#{row}") }
        have = cell(sheet, loose.first)
        item.problem = loose.size == 1 ? "#{loose.first} ohne $#{col}$#{row}" : "#{loose.size} Zellen ohne $#{col}$#{row}"
        item.message = "#{loose.first} rechnet nicht fest mit #{ref}#{have && have['formula'] ? ", dort steht #{q(german_formula(have['formula']))}" : ''}: Beim Kopieren rutscht der Bezug mit. " \
            "Schreib $#{col}$#{row}: Das $ hält Spalte und Zeile fest, wenn du die Formel nach unten ziehst."
        item
    end

    # the starting file's data unchanged (values and texts in these cells)
    def self.rule_same_data(rule, _body, starter, _all = nil, model = nil)
        item = Item.new(:rule => 'same_data', :label => 'Die Daten der Vorlage sind noch da', :ok => true, :marks => [])
        return item unless sheet?(starter)

        mine = sheet_of(model, rule)
        theirs = sheet_of(starter, rule)
        changed = Array(rule['same_data']).flat_map { |r| cells_of(r) }.reject do |n|
            a = cell(theirs, n)
            b = cell(mine, n)
            next b.nil? || b['type'] == 'empty' if a.nil?
            next false if b.nil?

            a['type'] == 'text' ? b['text'] == a['text'] || b['shown'] == a['shown'] : close?(b['value'], a['value'])
        end
        return item if changed.empty?

        item.ok = false
        item.marks = changed.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'geändert') }
        a = cell(theirs, changed.first)
        item.problem = changed.size == 1 ? "#{changed.first} geändert" : "#{changed.size} Zellen der Vorlage geändert"
        item.message = "In #{changed.first} stand in der Vorlage #{a ? q(shown(a)) : 'nichts'}#{changed.size > 1 ? " (und in #{changed.size - 1} weiteren Zellen etwas anderes)" : ''}. " \
            'Hier änderst du, wie die Tabelle rechnet und aussieht, nicht ihre Daten.'
        item
    end

    # number formats: currency, percent, date, or decimals: n
    def self.rule_number_format(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['number_format'] || {}
        sheet = sheet_of(model, rule)
        label = names(wants.map { |r, k| "#{r} als #{k.is_a?(Hash) ? "Zahl mit #{k['decimals']} Nachkommastellen" : FORMAT_NAMES[k] || k}" })
        item = Item.new(:rule => 'number_format', :label => label, :ok => true, :marks => [])
        wrong = []
        wants.each do |range, kind|
            cells_of(range).each do |n|
                c = cell(sheet, n)
                next if c.nil?

                f = c['number_format'] || {}
                ok = if kind.is_a?(Hash)
                    decimals = f['format'].to_s[/[.,](0+)/, 1].to_s.size
                    decimals == kind['decimals'].to_i
                elsif kind == 'currency'
                    # Excel writes its euro as text in the format (»#,##0.00 "€"«,
                    # also Buchhaltung), which LibreOffice reads as a plain number format
                    (f['type'].to_i & FORMAT_KINDS[kind]).positive? || f['format'].to_s.match?(/€|EUR|\[\$[^-\]]/)
                else
                    (f['type'].to_i & FORMAT_KINDS[kind].to_i).positive?
                end
                wrong << [n, kind] unless ok
            end
        end
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.map { |n, _| cell_mark(model, rule['sheet'].to_i, n, 'Zahlenformat') }
        n, kind = wrong.first
        want = kind.is_a?(Hash) ? "eine Zahl mit #{kind['decimals']} Nachkommastellen" : "#{FORMAT_NAMES[kind] || kind}"
        item.problem = if kind.is_a?(Hash)
            wrong.size == 1 ? "#{n} mit anderen Nachkommastellen" : "#{wrong.size} Zellen mit anderen Nachkommastellen"
        else
            wrong.size == 1 ? "#{n} nicht als #{FORMAT_NAMES[kind] || kind}" : "#{wrong.size} Zellen nicht als #{FORMAT_NAMES[kind] || kind}"
        end
        item.message = "#{wrong.size == 1 ? n : "#{wrong.size} Zellen, zum Beispiel #{n},"} #{wrong.size == 1 ? 'zeigt' : 'zeigen'} " \
            "#{q(shown(cell(sheet, n)))}, soll aber als #{want} formatiert sein. Das Format änderst du, nicht die Zahl: Die Zahl bleibt, wie sie ist, nur ihr Aussehen ändert sich."
        item
    end

    # no numbers typed as text (»12,50 €« typed with its sign): SUMME skips them
    def self.rule_text_numbers(rule, _body, _starter, _all = nil, model = nil)
        sheet = sheet_of(model, rule)
        item = Item.new(:rule => 'text_numbers', :label => 'Keine Zahlen als Text', :ok => true, :marks => [])
        found = Array(rule['text_numbers']).flat_map { |r| cells_of(r) }.select do |n|
            c = cell(sheet, n)
            c && c['type'] == 'text' && c['text'].to_s.strip.match?(/\A(€|EUR|Euro)?\s*[-+]?[\d.\s]*\d(,\d+)?\s*(€|%|EUR|Euro)?\z/)
        end
        return item if found.empty?

        item.ok = false
        item.marks = found.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'Text') }
        item.problem = found.size == 1 ? "Zahl als Text in #{found.first}" : "#{found.size} Zahlen als Text"
        item.message = "#{found.size == 1 ? "In #{found.first} steht" : "In #{found.size} Zellen, zum Beispiel #{found.first}, steht"} " \
            "#{q(cell(sheet, found.first)['text'])} als Text, nicht als Zahl: Mit ihr rechnet keine Formel, und SUMME lässt sie aus. " \
            'Tipp nur die Zahl und gib der Zelle ein Format, etwa Währung.'
        item
    end

    def self.rule_bold_cells(rule, _body, _starter, _all = nil, model = nil)
        sheet = sheet_of(model, rule)
        ranges = Array(rule['bold_cells'])
        item = Item.new(:rule => 'bold_cells', :label => "#{cells_label(ranges)} fett", :ok => true, :marks => [])
        plain = ranges.flat_map { |r| cells_of(r) }.select { |n| cell(sheet, n) && !cell(sheet, n)['bold'] }
        return item if plain.empty?

        item.ok = false
        item.marks = plain.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'nicht fett') }
        item.problem = plain.size == 1 ? "#{plain.first} nicht fett" : "#{plain.size} Zellen nicht fett"
        item.message = "#{plain.first} ist nicht fett. Die Zeile der Überschriften hebt sich ab, damit man sieht, was in jeder Spalte steht."
        item
    end

    # the rows of a range sorted by a column (and the same rows as in the
    # starting file, only in another order)
    def self.rule_sorted(rule, _body, starter, _all = nil, model = nil)
        spec = rule['sorted']
        sheet = sheet_of(model, rule)
        first, last = spec['range'].split(':')
        c1, r1 = cell_index(first)
        c2, r2 = cell_index(last)
        by = cell_index("#{spec['by']}1").first
        # then: a second column for rows equal in the first, ascending
        second = spec['then'] ? cell_index("#{spec['then']}1").first : nil
        desc = spec['order'] == 'desc'
        label = "Sortiert nach Spalte #{spec['by']}, #{desc ? 'absteigend' : 'aufsteigend'}#{second ? ", dann nach #{spec['then']}" : ''}"
        item = Item.new(:rule => 'sorted', :label => label, :ok => true, :marks => [])
        key_at = ->(s, col, r) { c = cell(s, cell_name(col, r)); c.nil? ? nil : (c['type'] == 'text' ? sort_key(c['text']) : c['value'].to_f) }
        key = ->(s, r) { key_at.(s, by, r) }
        rows = (r1..r2).to_a
        values = rows.map { |r| key.(sheet, r) }
        seconds = second ? rows.map { |r| key_at.(sheet, second, r) } : []
        bad = values.each_cons(2).each_with_index.find do |(a, b), i|
            next false if a.nil? || b.nil? || a.class != b.class
            next true if desc ? a < b : a > b
            next false unless second && a == b

            x, y = seconds[i], seconds[i + 1]
            !x.nil? && !y.nil? && x.class == y.class && x > y
        end
        # a row by its contents: numbers by value (a date formatted since is
        # the same row), texts as they are
        row_text = lambda do |s, r|
            (c1..c2).map do |c|
                x = cell(s, cell_name(c, r))
                x.nil? || x['type'] == 'empty' ? '' : x['type'] == 'text' ? x['text'].to_s : format('%.4f', x['value'].to_f)
            end
        end
        same = !sheet?(starter) || rows.map { |r| row_text.(sheet, r) }.sort == rows.map { |r| row_text.(sheet_of(starter, rule), r) }.sort
        return item if bad.nil? && same

        item.ok = false
        item.problem = same ? "Nicht nach Spalte #{spec['by']} sortiert" : 'Zeilen auseinandergerissen'
        if !same
            item.message = 'Die Zeilen sind nicht mehr dieselben wie in der Vorlage: Beim Sortieren ist eine Spalte allein mitgewandert. ' \
                'Markier die ganze Tabelle, bevor du sortierst, oder sortier über die Daten, dann bleiben die Zeilen zusammen.'
        else
            r = rows[bad[1] + 1]
            item.marks = [cell_mark(model, rule['sheet'].to_i, cell_name(by, r), 'nicht sortiert')]
            item.problem = "Zeile #{r} nicht in der Reihenfolge"
            item.message = if second && values[bad[1]] == values[bad[1] + 1]
                "#{cell_name(second, r)} (#{q(shown(cell(sheet, cell_name(second, r))))}) steht nach #{q(shown(cell(sheet, cell_name(second, rows[bad[1]]))))}; " \
                    "bei gleichem Wert in Spalte #{spec['by']} kommt es nach Spalte #{spec['then']} davor. Sortier mit einem zweiten Schlüssel."
            else
                "#{cell_name(by, r)} (#{q(shown(cell(sheet, cell_name(by, r))))}) steht nach #{q(shown(cell(sheet, cell_name(by, rows[bad[1]]))))}; " \
                    "nach Spalte #{spec['by']} #{desc ? 'absteigend' : 'aufsteigend'} sortiert käme es davor."
            end
        end
        item
    end

    def self.rule_autofilter(rule, _body, _starter, _all = nil, model = nil)
        on = (sheet_of(model, rule)['autofilter'] || {})['on']
        item = Item.new(:rule => 'autofilter', :label => 'Ein Filter über der Tabelle', :ok => on, :marks => [])
        item.problem = 'Noch kein Filter' unless on
        item.message = 'Die Tabelle hat noch keinen Filter. Mit ihm zeigst du nur die Zeilen, die du gerade brauchst, ohne eine zu löschen.' unless on
        item
    end

    # a conditional format on (part of) these cells; positive: true, it
    # colours exactly the cells above 0 (as far as its condition can be
    # worked out here: a comparison with a number; a formula or a colour
    # scale counts as right); shows: »Nachzahlung«, other: »Rückzahlung«, it
    # colours a cell that shows the one and not one that shows the other
    # (a condition on text: equal, contains …, or a formula A1="…")
    def self.rule_conditional(rule, _body, _starter, _all = nil, model = nil)
        range = rule['conditional'].to_s
        want = cells_of(range)
        sheet = sheet_of(model, rule)
        formats = sheet['conditional'] || []
        covered = formats.flat_map { |f| f['ranges'].flat_map { |r| cells_of(r) rescue [] } }
        ok = (want & covered).size >= [want.size / 2, 1].max
        item = Item.new(:rule => 'conditional', :label => "Bedingte Formatierung in #{range}", :ok => ok, :marks => [])
        unless ok
            item.problem = "Keine bedingte Formatierung in #{range}"
            item.message = "#{range} hat noch keine bedingte Formatierung. Mit ihr färbt das Programm eine Zelle selbst, je nachdem, was in ihr steht."
            return item
        end
        return text_conditional(item, rule, formats, want, model) if rule['shows']
        return item unless rule['positive']

        wrong = want.filter_map do |n|
            value = (cell(sheet, n) || {})['value']
            next if value.nil?

            coloured = coloured?(formats, n, value.to_f)
            next if coloured.nil? || coloured == (value.to_f > 0.005)

            [n, coloured]
        end
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.map { |n, c| cell_mark(model, rule['sheet'].to_i, n, c ? 'gefärbt, obwohl 0' : 'nicht gefärbt') }
        n, c = wrong.first
        shown_value = shown(cell(sheet, n))
        item.problem = c ? "#{n} gefärbt, obwohl nichts offen ist" : "#{n} nicht gefärbt, obwohl etwas offen ist"
        item.message = (c ? "Die bedingte Formatierung färbt auch #{n} (#{q(shown_value)}), wo nichts mehr offen ist." :
                            "Die bedingte Formatierung färbt #{n} (#{q(shown_value)}) nicht, wo noch etwas offen ist.") +
            ' Gefärbt werden sollen genau die Beträge über 0: die Bedingung »größer als 0«.'
        item
    end

    def self.text_conditional(item, rule, formats, cells, model)
        text = rule['shows'].to_s
        other = rule['other'].to_s
        n = cells.first
        coloured = coloured_text?(formats, n, text)
        plain = other.empty? ? false : coloured_text?(formats, n, other)
        return item if coloured != false && plain != true

        item.ok = false
        item.marks = [cell_mark(model, rule['sheet'].to_i, n, coloured == false ? "nicht gefärbt bei #{text}" : "gefärbt bei #{other}")]
        if coloured == false
            item.problem = "#{n} wird bei »#{text}« nicht gefärbt"
            item.message = "Die bedingte Formatierung in #{n} färbt die Zelle nicht, wenn dort »#{text}« steht. " \
                "Die Bedingung heißt »Zellwert ist gleich« mit \"#{text}\" in Anführungszeichen."
        else
            item.problem = "#{n} wird auch bei »#{other}« gefärbt"
            item.message = "Die bedingte Formatierung in #{n} färbt die Zelle auch, wenn dort »#{other}« steht. " \
                "Gefärbt werden soll sie nur bei »#{text}«: die Bedingung »Zellwert ist gleich« mit \"#{text}\"."
        end
        item
    end

    # Whether a cell's conditional formats colour it when it shows this
    # text: true, false, or nil when a condition cannot be worked out here.
    # Conditions on text (operator 0 =, 5 <>, 20 begins with, 21 ends with,
    # 22 contains, 23 does not contain, the text in quotes), and a formula
    # (24) of the form A1="text"
    def self.coloured_text?(formats, name, text)
        entries = formats.select { |f| f['ranges'].any? { |r| (cells_of(r) rescue []).include?(name) } }.flat_map { |f| f['entries'] || [] }
        return false if entries.empty?

        value = text.downcase
        results = entries.map do |e|
            f = e['formula1'].to_s.strip
            op = e['operator'].to_i
            next nil unless e['type'].to_i.zero?

            if op == 24
                m = f.match(/\A\$?[A-Z]{1,3}\$?\d+\s*=\s*"(.*)"\z/i)
                next nil unless m

                op = 0
                f = "\"#{m[1]}\""
            end
            next nil unless f.length >= 2 && f.start_with?('"') && f.end_with?('"')

            t = f[1..-2].downcase
            case op
            when 0 then value == t
            when 5 then value != t
            when 20 then value.start_with?(t)
            when 21 then value.end_with?(t)
            when 22 then value.include?(t)
            when 23 then !value.include?(t)
            end
        end
        return true if results.include?(true)
        return nil if results.include?(nil)

        false
    end

    # Whether a cell's conditional formats colour it at this value: true,
    # false, or nil when a condition cannot be worked out here
    # (com.sun.star.sheet.ConditionFormatOperator: 0 =, 1 <, 2 >, 3 <=,
    # 4 >=, 5 <>, 6 between, 7 not between; entry type 0 a condition)
    def self.coloured?(formats, name, value)
        entries = formats.select { |f| f['ranges'].any? { |r| (cells_of(r) rescue []).include?(name) } }.flat_map { |f| f['entries'] || [] }
        return false if entries.empty?

        results = entries.map do |e|
            next nil unless e['type'].to_i.zero? && e.key?('operator')

            a = e['formula1'].to_s.tr(',', '.')
            b = e['formula2'].to_s.tr(',', '.')
            next nil unless a.match?(/\A-?\d+(\.\d+)?\z/)

            x = a.to_f
            y = b.match?(/\A-?\d+(\.\d+)?\z/) ? b.to_f : x
            case e['operator'].to_i
            when 0 then value == x
            when 1 then value < x
            when 2 then value > x
            when 3 then value <= x
            when 4 then value >= x
            when 5 then value != x
            when 6 then value.between?([x, y].min, [x, y].max)
            when 7 then !value.between?([x, y].min, [x, y].max)
            end
        end
        return nil if results.include?(nil)

        results.any?
    end

    # a chart of this kind, with a title (and axis titles), showing this range
    def self.rule_chart(rule, _body, _starter, _all = nil, model = nil)
        spec = rule['chart']
        kind = spec['type'].to_s
        charts = (model['sheets'] || []).flat_map { |s| s['charts'] || [] }
        item = Item.new(:rule => 'chart', :label => "Ein #{CHART_NAMES[kind] || kind} mit Titel#{spec['axis_titles'] ? ' und Achsentiteln' : ''}", :ok => false, :marks => [])
        if charts.empty?
            item.problem = 'Noch kein Diagramm'
            item.message = "Die Tabelle hat noch kein Diagramm. Markier die Daten, dann Einfügen › Diagramm, und wähl ein #{CHART_NAMES[kind] || kind}."
            return item
        end
        chart = charts.find { |c| c['type'] == kind } || charts.first
        problems = []
        problems << "es ist ein #{CHART_NAMES[chart['type']] || chart['type']}, gefragt ist ein #{CHART_NAMES[kind] || kind}" if chart['type'] != kind
        problems << 'es hat keinen Titel' if spec['title'] && chart['title'].to_s.strip.empty?
        if spec['axis_titles'] && !%w[pie].include?(chart['type'])
            problems << 'eine Achse hat keinen Titel' if chart['x_title'].to_s.strip.empty? || chart['y_title'].to_s.strip.empty?
        end
        if spec['range']
            want = cells_of(spec['range'])
            have = Array(chart['ranges']).flat_map { |r| cells_of(r) rescue [] }
            problems << "es zeigt nicht die Daten aus #{spec['range']}" unless (want - have).empty?
            # a row below the data in the same columns: mostly the total,
            # which in a pie would be a second slice as big as all the rest
            cols = want.map { |n| cell_index(n).first }.uniq
            last = want.map { |n| cell_index(n).last }.max
            below = have.select { |n| c, r = cell_index(n); cols.include?(c) && r > last }
            problems << "es zeigt auch #{below.first}, eine Zeile unter den Daten (die Summe ist kein Teil des Ganzen)" unless below.empty?
        end
        item.ok = problems.empty?
        unless item.ok
            first = problems.first
            item.problem = if first.start_with?('es zeigt auch') then 'Das Diagramm zeigt auch die Summe'
                           elsif first.start_with?('es ist ein') then 'Ein anderes Diagramm'
                           elsif first.include?('keinen Titel') && first.start_with?('es') then 'Das Diagramm hat keinen Titel'
                           elsif first.include?('Achse') then 'Eine Achse ohne Titel'
                           else 'Das Diagramm zeigt andere Daten'
                           end
            item.message = "Dein Diagramm: #{problems.join(', ')}."
        end
        item
    end

    # no cell shows an error (#DIV/0!, #NAME?, #WERT!, #BEZUG!)
    def self.rule_no_errors(rule, _body, _starter, _all = nil, model = nil)
        sheet = sheet_of(model, rule)
        bad = (sheet['cells'] || []).select { |c| c['type'] == 'formula' && c['error'].to_i.positive? }
        item = Item.new(:rule => 'no_errors', :label => 'Keine Fehler in den Formeln', :ok => bad.empty?, :marks => [])
        return item if item.ok

        item.marks = bad.map { |c| cell_mark(model, rule.is_a?(Hash) ? rule['sheet'].to_i : 0, c['cell'], c['shown']) }
        c = bad.first
        item.problem = bad.size == 1 ? "#{c['cell']} zeigt #{c['shown']}" : "#{bad.size} Formeln mit Fehler"
        item.message = "#{c['cell']} zeigt #{q(c['shown'])}: Die Formel #{q(german_formula(c['formula']))} kann nicht rechnen. " \
            '#DIV/0! teilt durch null oder eine leere Zelle, #WERT! rechnet mit einem Text, #NAME? kennt einen Namen nicht ' \
            '(ein Tippfehler in der Funktion?), #BEZUG! zeigt auf eine gelöschte Zelle.'
        item
    end
end
