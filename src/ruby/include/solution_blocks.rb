require 'cgi'

# Code the students write themselves first (docs/aktivitaet-schreiben.md,
# »Jetzt bist du dran«). A box says what to write; the page's own code
# for it follows between two markers on lines of their own:
#
#   <div class='hint own'>
#   <strong>Jetzt bist du dran:</strong> Schreib <code>hit_floor</code> …
#   </div>
#
#   <!-- solution -->
#   ```c
#   double hit_floor(vec origin, vec dir)
#   …
#   ```
#   <!-- /solution -->
#
# Everything between the markers is folded away under »Lösung anzeigen«
# (<details class='solution'>) until a student opens it. It stays in the
# Markdown as it was, so the recipes, the teacher's step code (StageCode)
# and the check that the page shows the tested code (PageCode) see it as
# before, and the page goes on with this code like with any other.
module SolutionBlocks
    START = /^[ \t]*<!--[ \t]*solution[ \t]*-->[ \t]*$/
    FINISH = /^[ \t]*<!--[ \t]*\/solution[ \t]*-->[ \t]*$/
    SUMMARY = 'Lösung anzeigen'

    # The markers become empty marker elements that Redcarpet passes through;
    # wrap puts what is between them into one element after rendering.
    # Fenced code blocks are left alone, so pages can show the markers.
    def self.mark(markdown)
        markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(START, "\n<div class='solution-start'></div>\n")
                 .gsub(FINISH, "\n<div class='solution-end'></div>\n")
        end.join
    end

    # In a rendered page (Nokogiri), everything from a start marker to the
    # next end marker among its siblings goes into
    # <details class='solution'><summary>Lösung anzeigen</summary>…</details>.
    def self.wrap(root, warn: nil)
        root.css('div.solution-start').each do |start|
            details = Nokogiri::XML::Node.new('details', root.document)
            details['class'] = 'solution'
            summary = Nokogiri::XML::Node.new('summary', root.document)
            summary.content = SUMMARY
            details.add_child(summary)
            node = start.next_sibling
            closed = false
            while node
                following = node.next_sibling
                if node.element? && node['class'] == 'solution-end'
                    node.remove
                    closed = true
                    break
                end
                details.add_child(node)
                node = following
            end
            warn&.call('<!-- solution --> without <!-- /solution -->') unless closed
            start.replace(details)
        end
        root.css('div.solution-end').each do |stray|
            warn&.call('<!-- /solution --> without <!-- solution -->')
            stray.remove
        end
        root
    end
end
