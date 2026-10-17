require 'json'
require 'cgi'

# The tables of a dataset as schema lines, one per table:
#
#   movie_genre(↑movie_id, ↑genre_id)    with the primary key underlined
#
# _schema(countries) on a line of its own in a page becomes these lines, in
# a panel that is easy to find again (docs/aufgaben.md). With rows, each
# table also shows its first rows, right after an import, so there is
# something to ask about; new: names the tables that are new at this point,
# marked and open, while the known ones stay closed:
#
#   _schema(countries-basic, rows)
#   _schema(countries, rows, new: border currency language country_language)
# The tables, their columns and keys come from the JSON
# the Query Stepper uses (src/static/query-stepper/<name>.json, written by
# src/scripts/query-stepper-data.rb), so page and tool always agree.
module Schemas
    ROOT = File.expand_path('../../static/query-stepper', __dir__)
    LINE = /^_schema\(([a-z0-9-]+)((?:,[^)\n]*)?)\)[ \t]*$/
    ROWS = 3

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # The tables of a dataset: { name => { 'columns', 'key', 'references' } }, or nil
    def self.tables(name, root: ROOT)
        path = File.join(root, "#{name}.json")
        return nil unless File.file?(path)

        JSON.parse(File.read(path))['tables']
    end

    # One table as a line: the primary key underlined, a foreign key with ↑
    def self.line(name, table)
        key = table['key'] || []
        references = table['references'] || {}
        columns = table['columns'].map do |column|
            text = h(column)
            if references[column]
                text = "<span class='schema-fk' title='verweist auf #{h(references[column])}'><span class='schema-arrow'>↑</span>#{text}</span>"
            end
            key.include?(column) ? "<u>#{text}</u>" : text
        end
        "<div><span class='schema-table'>#{h(name)}</span>(#{columns.join(', ')})</div>"
    end

    # A value as mycli shows it: 652230.0 as 652230, NULL for nothing
    def self.value(v)
        return 'NULL' if v.nil?
        return v.to_i.to_s if v.is_a?(Float) && v == v.floor

        v.to_s
    end

    # The first rows of a table, and how many there are in all
    def self.rows_html(table)
        rows = table['rows'] || []
        head = table['columns'].map { |c| "<th>#{h(c)}</th>" }.join
        body = rows.first(ROWS).map { |r| "<tr>#{r.map { |v| "<td>#{h(value(v))}</td>" }.join}</tr>" }.join
        more = rows.size > ROWS ? "<div class='schema-more'>… und #{rows.size - ROWS} weitere Zeilen, #{rows.size} in allen</div>" : ''
        "<div class='schema-rows'><table class='table table-sm'><thead><tr>#{head}</tr></thead><tbody>#{body}</tbody></table></div>#{more}"
    end

    def self.html(name, root: ROOT, rows: false, fresh: [])
        tables = tables(name, root: root)
        return nil unless tables

        io = +"<div class='schema-panel'><div class='schema-panel-title'><i class='bi bi-table' aria-hidden='true'></i>Die Tabellen</div>"
        if rows
            tables.each do |table, t|
                new_one = fresh.include?(table)
                open = fresh.empty? || new_one
                badge = new_one ? "<span class='schema-new'>neu</span>" : ''
                io << "<details class='schema-lines schema-table-rows'#{open ? ' open' : ''}><summary>#{line(table, t).sub('</div>', "#{badge}</div>")}</summary>#{rows_html(t)}</details>"
            end
        else
            io << "<div class='schema-lines' aria-label='Die Tabellen von #{h(name)}'>#{tables.map { |table, t| line(table, t) }.join}</div>"
        end
        io << '</div>'
    end

    # _schema(name[, rows][, new: a b]) on a line of its own; an unknown name
    # or table is reported, an unknown name left as it is.
    def self.expand(markdown, root: ROOT, warn: nil)
        markdown.gsub(LINE) do
            whole, name, options = $&, $1, $2.to_s
            rows = options =~ /(\A|,)\s*rows\s*(,|\z)/ ? true : false
            fresh = options[/new:\s*([a-z0-9_ ]+)/, 1].to_s.split
            html = html(name, root: root, rows: rows, fresh: fresh)
            if html
                unknown = fresh - tables(name, root: root).keys
                warn&.call("_schema(#{name}): no table #{unknown.join(', ')}") unless unknown.empty?
            else
                warn&.call("_schema(#{name}): no such dataset")
            end
            html || whole
        end
    end
end
