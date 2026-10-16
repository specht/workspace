require 'json'
require 'cgi'

# The tables of a dataset as schema lines, one per table:
#
#   movie_genre(↑movie_id, ↑genre_id)    with the primary key underlined
#
# _schema(terra1) on a line of its own in a page becomes these lines
# (docs/aufgaben.md). The tables, their columns and keys come from the JSON
# the SQL-Zeitlupe uses (src/static/query-stepper/<name>.json, written by
# src/scripts/query-stepper-data.rb), so page and tool always agree.
module Schemas
    ROOT = File.expand_path('../../static/query-stepper', __dir__)
    LINE = /^_schema\(([a-z0-9-]+)\)[ \t]*$/

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

    def self.html(name, root: ROOT)
        tables = tables(name, root: root)
        return nil unless tables

        lines = tables.map { |table, t| line(table, t) }
        "<div class='schema-lines' aria-label='Die Tabellen von #{h(name)}'>#{lines.join}</div>"
    end

    # _schema(name) on a line of its own; an unknown name is reported and left.
    def self.expand(markdown, root: ROOT, warn: nil)
        markdown.gsub(LINE) do
            html = html($1, root: root)
            warn&.call("_schema(#{$1}): no such dataset") unless html
            html || $&
        end
    end
end
