require 'cgi'

# Tips in steps (docs/didaktik.md, docs/aufgaben.md »Tipps in Stufen«).
#
# A task's hint may be a list instead of one text, and an »Erst selbst«
# box (SolutionBlocks) may have tips before its solution, each between two
# markers on lines of their own:
#
#   <!-- hint -->
#   Was wäre `t` für einen Strahl, der genau nach unten zeigt?
#   <!-- /hint -->
#   <!-- hint -->
#   Im Code: `double t = …;`, dann eine Bedingung wie in `hit_sphere`.
#   <!-- /hint -->
#
# Both become the same markup: one <details> per step, »Tipp 1 von 3« and so
# on, in a <div class='hint-steps'>. hint_steps.js shows only the first; the
# next appears DELAY_SECONDS after the one before was opened, so there is
# time to think about each tip. What has been opened is kept in the
# browser only (localStorage), so a reload does not start the waiting again;
# nothing goes to the server, and no teacher sees who needed which tip.
# Without JavaScript all steps are there at once.
#
# The steps lead towards the solution but never contain all of it: the
# last step is a scaffold or the one missing idea, not the finished code.
module HintSteps
    MAX_STEPS = 5
    DELAY_SECONDS = 60
    START = /^[ \t]*<!--[ \t]*hint[ \t]*-->[ \t]*$/
    FINISH = /^[ \t]*<!--[ \t]*\/hint[ \t]*-->[ \t]*$/

    def self.summary(index, count)
        "Tipp #{index + 1} von #{count}"
    end

    # The steps, already rendered to HTML, as one block.
    def self.html(steps, extra_class: nil)
        classes = ['hint-step', extra_class].compact.join(' ')
        io = +"<div class='hint-steps' data-delay='#{DELAY_SECONDS}'>"
        steps.each_with_index do |step, i|
            io << "<details class='#{classes}'><summary>#{CGI.escapeHTML(summary(i, steps.size))}</summary>#{step}</details>"
        end
        io << '</div>'
    end

    # The markers become empty marker elements that Redcarpet passes through;
    # wrap puts what is between them into one step after rendering. Fenced
    # code blocks are left alone, so pages can show the markers.
    def self.mark(markdown)
        markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(START, "\n<div class='hint-start'></div>\n")
                 .gsub(FINISH, "\n<div class='hint-end'></div>\n")
        end.join
    end

    # In a rendered page (Nokogiri): every step into a <details>, and steps
    # that follow each other into one <div class='hint-steps'>, numbered.
    def self.wrap(root, warn: nil)
        root.css('div.hint-start').each do |start|
            step = Nokogiri::XML::Node.new('details', root.document)
            step['class'] = 'hint-step'
            node = start.next_sibling
            closed = false
            while node
                following = node.next_sibling
                if node.element? && node['class'] == 'hint-end'
                    node.remove
                    closed = true
                    break
                end
                step.add_child(node)
                node = following
            end
            warn&.call('<!-- hint --> without <!-- /hint -->') unless closed
            start.replace(step)
        end
        root.css('div.hint-end').each do |stray|
            warn&.call('<!-- /hint --> without <!-- hint -->')
            stray.remove
        end
        root.css('details.hint-step').each do |step|
            next if step.parent.nil? || step.parent['class'].to_s.split.include?('hint-steps')

            group = [step]
            node = step.next_element
            while node && node.name == 'details' && node['class'] == 'hint-step'
                group << node
                node = node.next_element
            end
            box = Nokogiri::XML::Node.new('div', root.document)
            box['class'] = 'hint-steps'
            box['data-delay'] = DELAY_SECONDS.to_s
            step.add_previous_sibling(box)
            group.each_with_index do |member, i|
                summary = Nokogiri::XML::Node.new('summary', root.document)
                summary.content = summary(i, group.size)
                member.children.first ? member.children.first.add_previous_sibling(summary) : member.add_child(summary)
                box.add_child(member)
            end
        end
        root
    end
end
