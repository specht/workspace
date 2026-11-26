require 'cgi'

# A reference to look things up in while working (»Auf einen Blick« in the
# Routenplaner: what the functions get and return), in a panel that slides in
# from the right instead of a chapter at the end of the page:
#
#   <!-- side-panel: auf-einen-blick Auf einen Blick -->
#   Was deine Funktionen bekommen und zurückgeben …
#   <!-- /side-panel -->
#
# It stays in the Markdown as it was (recipes, StageCode and PageCode see it
# as before) and becomes a tab on the right edge of the window and the panel
# (a Bootstrap Offcanvas, side_panel.js):
#
#   <div class='side-panel-box'>
#   <button class='side-panel-tab' …>Auf einen Blick</button>
#   <aside class='side-panel offcanvas offcanvas-end' id='auf-einen-blick'>…</aside>
#   </div>
#
# Links to #auf-einen-blick open the panel. side_panel.js moves the box to the
# end of the body, so that a meeting point (»Treffpunkt«), which dims
# everything after it, never dims the panel, and copies the container's
# data-code-language onto it, which shows the parts for the chosen language.
module SidePanel
    START = /^[ \t]*<!--[ \t]*side-panel:[ \t]*([a-z0-9][a-z0-9-]*)[ \t]+(\S.*?)[ \t]*-->[ \t]*$/
    FINISH = /^[ \t]*<!--[ \t]*\/side-panel[ \t]*-->[ \t]*$/

    # The markers become empty marker elements that Redcarpet passes through;
    # wrap puts what is between them into the panel after rendering. Fenced
    # code blocks are left alone, so pages can show the markers.
    def self.mark(markdown)
        markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(START) { "\n<div class='side-panel-start' data-id='#{Regexp.last_match(1)}' data-title='#{CGI.escapeHTML(Regexp.last_match(2))}'></div>\n" }
                 .gsub(FINISH, "\n<div class='side-panel-end'></div>\n")
        end.join
    end

    def self.wrap(root, warn: nil)
        root.css('div.side-panel-start').each do |start|
            id = start['data-id']
            title = start['data-title']
            doc = root.document
            box = Nokogiri::HTML::DocumentFragment.new(doc, <<~HTML).children.find(&:element?)
                <div class='side-panel-box'>
                <button type='button' class='side-panel-tab' data-bs-toggle='offcanvas' data-bs-target='##{id}' aria-controls='#{id}'><i class='bi bi-journal-code' aria-hidden='true'></i><span>#{CGI.escapeHTML(title)}</span></button>
                <aside class='side-panel offcanvas offcanvas-end' id='#{id}' tabindex='-1' data-bs-scroll='true' data-bs-backdrop='false' aria-labelledby='#{id}-title'>
                <div class='offcanvas-header'><div class='side-panel-title' id='#{id}-title'><i class='bi bi-journal-code' aria-hidden='true'></i> #{CGI.escapeHTML(title)}</div><button type='button' class='btn-close' data-bs-dismiss='offcanvas' aria-label='Schließen'></button></div>
                <div class='offcanvas-body side-panel-body'></div>
                </aside>
                <script src='/include/side_panel.js'></script>
                </div>
            HTML
            body = box.at_css('.side-panel-body')
            node = start.next_sibling
            closed = false
            while node
                following = node.next_sibling
                if node.element? && node['class'] == 'side-panel-end'
                    node.remove
                    closed = true
                    break
                end
                body.add_child(node)
                node = following
            end
            warn&.call("<!-- side-panel: #{id} … --> without <!-- /side-panel -->") unless closed
            start.replace(box)
        end
        root.css('div.side-panel-end').each do |stray|
            warn&.call('<!-- /side-panel --> without <!-- side-panel: … -->')
            stray.remove
        end
        root
    end
end
