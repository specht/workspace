require 'cgi'

# Tools of the site that pages send students to: the TCP-Konsole,
# Automatron and the query stepper. They live outside the activity, so every link to them opens a
# new tab, and the page the student is working on stays where it was.
#
# Both need a login. They are not in the menu: a page shows one where it is
# needed,
# - _tool(name) on a line of its own in a page becomes a box with icon,
#   name, what it does and »Öffnen« (expand),
# - any other link to a tool in a page opens a new tab (link_targets).
module Tools
    LIST = {
        'tcp-console' => {
            :title => 'TCP-Konsole',
            :path => '/tcp-console',
            :icon => 'bi-chat-left-text',
            :text => 'Eine TCP-Verbindung Zeile für Zeile tippen, wie mit nc, auf Wunsch mit TLS',
        },
        'automatron' => {
            :title => 'Automatron',
            :path => '/automatron',
            :icon => 'bi-diagram-3',
            :text => 'Aus einem regulären Ausdruck Schritt für Schritt Automaten zeichnen',
        },
        'query-stepper' => {
            :title => 'SQL-Zeitlupe',
            :path => '/query-stepper',
            :icon => 'bi-table',
            :text => 'Eine SQL-Abfrage Schritt für Schritt ausführen, mit der Tabelle nach jedem Schritt',
        },
    }.freeze

    PATHS = LIST.values.map { |tool| tool[:path] }.freeze

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.card(name)
        tool = LIST[name]
        return nil unless tool

        "<a class='tool-card' href='#{tool[:path]}' target='_blank' rel='noopener'>" \
            "<i class='bi #{tool[:icon]} tool-card-icon' aria-hidden='true'></i>" \
            "<span class='tool-card-text'><strong>#{h(tool[:title])}</strong><span>#{h(tool[:text])}</span></span>" \
            "<span class='tool-card-open'>Öffnen<i class='bi bi-box-arrow-up-right' aria-hidden='true'></i>" \
            "<span class='visually-hidden'> (in einem neuen Tab)</span></span></a>"
    end

    # _tool(name) on a line of its own; an unknown name is reported and left.
    def self.expand(markdown, warn: nil)
        markdown.gsub(/^_tool\(([a-z0-9-]+)\)[ \t]*$/) do
            html = card($1)
            warn&.call("_tool(#{$1}): no such tool") unless html
            html ? "\n#{html}\n" : $&
        end
    end

    # Links to a tool in a rendered page (Nokogiri) open a new tab.
    def self.link_targets(root)
        root.css('a[href]').each do |a|
            path = a['href'].to_s.split(/[?#]/).first
            next unless PATHS.include?(path)

            a['target'] = '_blank'
            a['rel'] = 'noopener'
        end
    end
end
