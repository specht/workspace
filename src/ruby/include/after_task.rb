require 'cgi'

# What gives away the answer to a task, above all to a prediction (a
# screenshot of the result, the paragraph that explains it), waits until the
# task has been answered (docs/didaktik.md, »Vermuten vor dem Ausprobieren«):
#
#   _task(stufen)
#
#   <!-- after: stufen -->
#   <!-- tutorial-screenshot … -->
#   <img src='licht.webp' alt='…'>
#
#   Nach hinten wird es dunkel …
#   <!-- /after -->
#
# It stays in the Markdown as it was (recipes, StageCode and PageCode see it
# as before) and becomes
#
#   <div class='after-task' data-after='stufen'>
#   <p class='after-task-wait'>Hier geht es weiter, …</p>
#   <div class='after-task-body'>…</div>
#   </div>
#
# For everyone who can answer, teachers too, Main#task_page_setup hides the
# body (and shows the line instead) until tasks.js finds an answer to a
# prediction, or a solution to any other task; a teacher can open it with a
# button in the line. Visitors, classes without tasks and pages without
# JavaScript see everything at once.
module AfterTask
    START = /^[ \t]*<!--[ \t]*after:[ \t]*([A-Za-z0-9_-]+)[ \t]*-->[ \t]*$/
    FINISH = /^[ \t]*<!--[ \t]*\/after[ \t]*-->[ \t]*$/
    WAIT = 'Hier geht es weiter, sobald du deine Antwort oben abgegeben hast.'

    # The ids of the tasks a page waits for, in order.
    def self.ids(markdown)
        prose(markdown).flat_map { |piece| piece.scan(START).flatten }
    end

    # The markers become empty marker elements that Redcarpet passes through;
    # wrap puts what is between them into one element after rendering.
    # Fenced code blocks are left alone, so pages can show the markers.
    def self.mark(markdown)
        markdown.split(/(^```.*?^```[ \t]*$)/m).map.with_index do |piece, i|
            next piece if i.odd?

            piece.gsub(START) { "\n<div class='after-task-start' data-after='#{Regexp.last_match(1)}'></div>\n" }
                 .gsub(FINISH, "\n<div class='after-task-end'></div>\n")
        end.join
    end

    def self.wrap(root, warn: nil)
        root.css('div.after-task-start').each do |start|
            box = Nokogiri::XML::Node.new('div', root.document)
            box['class'] = 'after-task'
            box['data-after'] = start['data-after']
            wait = Nokogiri::XML::Node.new('p', root.document)
            wait['class'] = 'after-task-wait'
            wait.content = WAIT
            body = Nokogiri::XML::Node.new('div', root.document)
            body['class'] = 'after-task-body'
            box.add_child(wait)
            box.add_child(body)
            node = start.next_sibling
            closed = false
            while node
                following = node.next_sibling
                if node.element? && node['class'] == 'after-task-end'
                    node.remove
                    closed = true
                    break
                end
                body.add_child(node)
                node = following
            end
            warn&.call("<!-- after: #{start['data-after']} --> without <!-- /after -->") unless closed
            start.replace(box)
        end
        root.css('div.after-task-end').each do |stray|
            warn&.call('<!-- /after --> without <!-- after: … -->')
            stray.remove
        end
        root
    end

    def self.prose(markdown)
        markdown.split(/(^```.*?^```[ \t]*$)/m).each_with_index.reject { |_, i| i.odd? }.map(&:first)
    end
end
