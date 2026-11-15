require 'json'

# A trace table (docs/aufgaben.md, »trace«): the values of the variables of
# a loop, one row per pass, filled in by the student and compared cell by
# cell. The code stands on the page above the card; the table is the same
# in every language, so the task needs no variants.
#
#   check:
#     type: trace
#     columns: [i, total]      # the variables, as the code names them
#     rows:                    # what each pass leaves in them, at its end
#       - [1, 1]
#       - [2, 3]
#     given: [[0, 0]]          # cells shown filled in: [row, column], from 0
#     row_label: Durchlauf     # the heading of the first column (default)
#
# The answer is JSON, a list of rows of texts, given cells included. A
# number counts as written the German or the English way (3,5 and 3.5);
# any other value (true, a text) must match as written, without quotes.
module TraceCheck
    SPEC_KEYS = %w[columns rows given row_label].freeze
    MAX_ROWS = 20
    MAX_COLUMNS = 6

    def self.spec_errors(check)
        errors = []
        columns = check['columns']
        rows = check['rows']
        unless columns.is_a?(Array) && columns.size.between?(1, MAX_COLUMNS) && columns.all? { |c| c.is_a?(String) && !c.strip.empty? }
            return ["trace needs columns: 1 to #{MAX_COLUMNS} names of variables"]
        end
        unless rows.is_a?(Array) && rows.size.between?(1, MAX_ROWS) && rows.all? { |r| r.is_a?(Array) && r.size == columns.size }
            return ["trace needs rows: 1 to #{MAX_ROWS} rows of #{columns.size} values each"]
        end
        errors << 'trace values are numbers, true, false or short texts' unless rows.flatten.all? { |v| [Integer, Float, String, TrueClass, FalseClass].any? { |k| v.is_a?(k) } && v.to_s.size <= 40 }
        given = check['given'] || []
        if !given.is_a?(Array) || !given.all? { |g| g.is_a?(Array) && g.size == 2 && g.all?(Integer) && g[0].between?(0, rows.size - 1) && g[1].between?(0, columns.size - 1) }
            errors << 'given must be a list of [row, column] inside the table'
        elsif given.size >= rows.size * columns.size
            errors << 'given leaves nothing to fill in'
        end
        errors << 'row_label must be a short text' if check.key?('row_label') && !(check['row_label'].is_a?(String) && check['row_label'].size.between?(1, 20))
        errors
    end

    def self.given?(check, row, column)
        Array(check['given']).include?([row, column])
    end

    # A value as it is compared: a number as a number, anything else as text
    def self.normal(value)
        text = value.to_s.strip
        return nil if text.empty?
        return Float(text.tr(',', '.')).round(9) if text.match?(/\A[-+]?\d+([.,]\d+)?\z/)

        text.delete_prefix('"').delete_suffix('"').delete_prefix("'").delete_suffix("'")
    end

    # The table filled in completely and correctly, as an answer
    def self.correct_answer(check)
        JSON.generate(check['rows'].map { |row| row.map(&:to_s) })
    end

    # [status, message, read]
    def self.check(task, check, answer)
        cells = begin
            JSON.parse(answer.to_s)
        rescue JSON::ParserError
            nil
        end
        rows = check['rows']
        columns = check['columns']
        unless cells.is_a?(Array) && cells.size == rows.size && cells.all? { |r| r.is_a?(Array) && r.size == columns.size }
            return [:invalid, 'Füll die Tabelle aus.', nil]
        end

        empty = 0
        wrong = []
        rows.each_with_index do |row, r|
            row.each_with_index do |value, c|
                next if given?(check, r, c)

                mine = normal(cells[r][c])
                if mine.nil?
                    empty += 1
                elsif mine != normal(value)
                    wrong << [r, c]
                end
            end
        end
        return [:invalid, empty == 1 ? 'Ein Feld ist noch leer.' : "#{empty} Felder sind noch leer.", nil] if empty.positive?
        return [:correct, nil, nil] if wrong.empty?

        r, c = wrong.first
        label = check['row_label'] || 'Durchlauf'
        own = (task['feedback'] || []).find do |entry|
            w = entry['when'] || {}
            w['cell'] == [r, c] && normal(w['equals']) == normal(cells[r][c])
        end
        where = "#{label} #{r + 1}, `#{columns[c]}`"
        message = if own
            "#{where}: #{own['say']}"
        else
            more = wrong.size > 1 ? " Insgesamt stimmen #{wrong.size} Felder noch nicht." : ''
            "#{where} stimmt noch nicht. Geh den Code dafür Zeile für Zeile durch.#{more}"
        end
        [:wrong, message, nil]
    end
end
