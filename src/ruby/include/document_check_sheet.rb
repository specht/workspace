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
                     sorted autofilter conditional chart no_errors].freeze

    # German names of the functions a class meets (LibreOffice and Excel
    # show them so; the file stores the English ones)
    FUNCTION_NAMES = { 'SUM' => 'SUMME', 'AVERAGE' => 'MITTELWERT', 'MAX' => 'MAX', 'MIN' => 'MIN', 'COUNT' => 'ANZAHL',
                       'COUNTA' => 'ANZAHL2', 'IF' => 'WENN', 'COUNTIF' => 'ZÄHLENWENN', 'SUMIF' => 'SUMMEWENN',
                       'ROUND' => 'RUNDEN', 'TODAY' => 'HEUTE' }.freeze
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
        first = cell(sheet, typed.first)
        item.message = "#{typed.size == 1 ? "In #{typed.first} steht" : "In #{typed.size} Zellen, zum Beispiel #{typed.first}, steht"} " \
            "#{first ? "eine getippte #{first['type'] == 'text' ? 'Angabe' : 'Zahl'} (#{q(shown(first))})" : 'nichts'}, keine Formel. " \
            'Ändert sich eine Zahl, mit der sie gerechnet wurde, bleibt sie falsch. Eine Formel beginnt mit =, zum Beispiel =B2*C2.'
        item
    end

    # these cells show these values
    def self.rule_values(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['values'] || {}
        sheet = sheet_of(model, rule)
        item = Item.new(:rule => 'values', :label => "Die Ergebnisse in #{cells_label(wants.keys)} stimmen", :ok => true, :marks => [])
        wrong = wants.reject { |n, v| close?((cell(sheet, n) || {})['value'], v) }
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'falsches Ergebnis') }
        n, v = wrong.first
        have = cell(sheet, n)
        item.message = "In #{n} steht #{have ? q(shown(have)) : 'nichts'}, richtig wäre #{number(v)}." +
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
        wrong = expect.reject { |n, v| close?((probe['values'] || {})[n], v) }
        return item if wrong.empty? && probe['error'].nil?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'rechnet nicht mit') }
        n, v = wrong.first || expect.first
        got = (probe['shown'] || {})[n]
        item.message = "Ich habe #{changes} geändert. Dann müsste in #{n} #{number(v)} stehen, " \
            "es steht aber #{got.to_s.empty? ? 'nichts' : q(got)} da. Rechnet die Zelle mit einer getippten Zahl statt mit der Zelle?"
        item
    end

    # these cells use a function (SUMME, MITTELWERT …)
    def self.rule_functions(rule, _body, _starter, _all = nil, model = nil)
        wants = rule['functions'] || {}
        sheet = sheet_of(model, rule)
        label = names(wants.map { |n, f| "#{n} mit #{FUNCTION_NAMES[f.to_s.upcase] || f}" })
        item = Item.new(:rule => 'functions', :label => label, :ok => true, :marks => [])
        wrong = wants.reject { |n, f| (cell(sheet, n) || {})['formula'].to_s.upcase.include?("#{f.to_s.upcase}(") }
        return item if wrong.empty?

        item.ok = false
        item.marks = wrong.keys.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'ohne die Funktion') }
        n, f = wrong.first
        have = cell(sheet, n)
        name = FUNCTION_NAMES[f.to_s.upcase] || f
        item.message = "#{n} soll mit #{name} rechnen#{have && have['formula'] ? ", dort steht #{q(german_formula(have['formula']))}" : ''}. " \
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
        item.message = "#{loose.first} soll mit #{ref} rechnen, fest, auch nach dem Kopieren#{have && have['formula'] ? "; dort steht #{q(german_formula(have['formula']))}" : ''}. " \
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
            c && c['type'] == 'text' && c['text'].to_s.strip.match?(/\A[-+]?[\d.\s]*\d(,\d+)?\s*(€|%|EUR|Euro)?\z/)
        end
        return item if found.empty?

        item.ok = false
        item.marks = found.map { |n| cell_mark(model, rule['sheet'].to_i, n, 'Text') }
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
        desc = spec['order'] == 'desc'
        item = Item.new(:rule => 'sorted', :label => "Sortiert nach Spalte #{spec['by']}, #{desc ? 'absteigend' : 'aufsteigend'}", :ok => true, :marks => [])
        key = ->(s, r) { c = cell(s, cell_name(by, r)); c.nil? ? nil : (c['type'] == 'text' ? c['text'].to_s.downcase : c['value'].to_f) }
        rows = (r1..r2).to_a
        values = rows.map { |r| key.(sheet, r) }
        bad = values.each_cons(2).each_with_index.find { |(a, b), _| !a.nil? && !b.nil? && a.class == b.class && (desc ? a < b : a > b) }
        row_text = ->(s, r) { (c1..c2).map { |c| shown(cell(s, cell_name(c, r))) } }
        same = !sheet?(starter) || rows.map { |r| row_text.(sheet, r) }.sort == rows.map { |r| row_text.(sheet_of(starter, rule), r) }.sort
        return item if bad.nil? && same

        item.ok = false
        if !same
            item.message = 'Die Zeilen sind nicht mehr dieselben wie in der Vorlage: Beim Sortieren ist eine Spalte allein mitgewandert. ' \
                'Markier die ganze Tabelle, bevor du sortierst, oder sortier über die Daten, dann bleiben die Zeilen zusammen.'
        else
            r = rows[bad[1] + 1]
            item.marks = [cell_mark(model, rule['sheet'].to_i, cell_name(by, r), 'nicht sortiert')]
            item.message = "#{cell_name(by, r)} (#{q(shown(cell(sheet, cell_name(by, r))))}) steht nach #{q(shown(cell(sheet, cell_name(by, rows[bad[1]]))))}; " \
                "nach Spalte #{spec['by']} #{desc ? 'absteigend' : 'aufsteigend'} sortiert käme es davor."
        end
        item
    end

    def self.rule_autofilter(rule, _body, _starter, _all = nil, model = nil)
        on = (sheet_of(model, rule)['autofilter'] || {})['on']
        item = Item.new(:rule => 'autofilter', :label => 'Ein Filter über der Tabelle', :ok => on, :marks => [])
        item.message = 'Die Tabelle hat noch keinen Filter. Mit ihm zeigst du nur die Zeilen, die du gerade brauchst, ohne eine zu löschen.' unless on
        item
    end

    # a conditional format on (part of) these cells
    def self.rule_conditional(rule, _body, _starter, _all = nil, model = nil)
        range = rule['conditional'].to_s
        want = cells_of(range)
        covered = (sheet_of(model, rule)['conditional'] || []).flat_map { |f| f['ranges'].flat_map { |r| cells_of(r) rescue [] } }
        ok = (want & covered).size >= [want.size / 2, 1].max
        item = Item.new(:rule => 'conditional', :label => "Bedingte Formatierung in #{range}", :ok => ok, :marks => [])
        item.message = "#{range} hat noch keine bedingte Formatierung. Mit ihr färbt das Programm eine Zelle selbst, je nachdem, was in ihr steht." unless ok
        item
    end

    # a chart of this kind, with a title (and axis titles), showing this range
    def self.rule_chart(rule, _body, _starter, _all = nil, model = nil)
        spec = rule['chart']
        kind = spec['type'].to_s
        charts = (model['sheets'] || []).flat_map { |s| s['charts'] || [] }
        item = Item.new(:rule => 'chart', :label => "Ein #{CHART_NAMES[kind] || kind} mit Titel#{spec['axis_titles'] ? ' und Achsentiteln' : ''}", :ok => false, :marks => [])
        if charts.empty?
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
        end
        item.ok = problems.empty?
        item.message = "Dein Diagramm: #{problems.join(', ')}." unless item.ok
        item
    end

    # no cell shows an error (#DIV/0!, #NAME?, #REF!)
    def self.rule_no_errors(rule, _body, _starter, _all = nil, model = nil)
        sheet = sheet_of(model, rule)
        bad = (sheet['cells'] || []).select { |c| c['type'] == 'formula' && c['error'].to_i.positive? }
        item = Item.new(:rule => 'no_errors', :label => 'Keine Fehler in den Formeln', :ok => bad.empty?, :marks => [])
        return item if item.ok

        item.marks = bad.map { |c| cell_mark(model, rule.is_a?(Hash) ? rule['sheet'].to_i : 0, c['cell'], c['shown']) }
        c = bad.first
        item.message = "#{c['cell']} zeigt #{q(c['shown'])}: Die Formel #{q(german_formula(c['formula']))} kann nicht rechnen. " \
            '#DIV/0! teilt durch null oder eine leere Zelle, #NAME? kennt einen Namen nicht (ein Tippfehler in der Funktion?), #REF! zeigt auf eine gelöschte Zelle.'
        item
    end
end
