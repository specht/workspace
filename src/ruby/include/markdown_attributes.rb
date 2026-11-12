# Redcarpet ends an HTML tag in Markdown at its first >, also inside a
# quoted attribute (alt='E33>=4 …'), and escapes the rest; the browser then
# reads the attribute on to the next quote, far down the page, and swallows
# text and task cards on the way. Before rendering, < and > inside the
# quoted values of HTML tags become &lt; and &gt;: the browser shows the
# same text. Code blocks and code spans stay as they are.
module MarkdownAttributes
    # a whole tag, its quoted values may contain < and >
    TAG = /<[a-zA-Z][a-zA-Z0-9-]*(?:\s+[\w:.-]+(?:\s*=\s*(?:'[^'\n]*'|"[^"\n]*"|[^\s'"<>]+))?)*\s*\/?>/.freeze
    VALUE = /'[^'\n]*'|"[^"\n]*"/.freeze

    def self.escape(markdown)
        fence = nil
        markdown.each_line.map do |line|
            if (m = line.match(/\A\s*(`{3,}|~{3,})/))
                fence = fence.nil? ? m[1][0] : (m[1][0] == fence ? nil : fence)
                next line
            end
            next line if fence || !line.include?('<')

            # outside code spans only
            line.split(/(`+[^`]*`+)/).map do |part|
                part.start_with?('`') ? part : part.gsub(TAG) { |tag| tag.gsub(VALUE) { |v| v.gsub('<', '&lt;').gsub('>', '&gt;') } }
            end.join
        end.join
    end
end
