require 'cgi'
require 'digest'

# Katas handed in to be shown in class (Einreichen, docs/plan-katas.md):
# under a solved Kata a student may hand in a copy of their code, and take
# it back. Never automatic: what others see is their own choice. On the
# class board (»Fortschritt«) the teacher finds them per Kata, numbered
# without names in an order of their own, and opens them one by one in a
# viewer large enough for the projector (kata_hand_ins.js); the names stay
# hidden until the teacher switches them on.
#
# Plain Ruby, tested on its own (test/kata_hand_ins_test.rb).
module KataHandIns
    NOTE = 'Lösungen, die Lernende zum Besprechen eingereicht haben: eine kurze, eine ungewöhnliche, eine schlaue ' \
        'oder eine mit einem interessanten Fehler. Sie muss nichts Besonderes sein. Zum Zeigen ohne Namen.'

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    # entries: [{ email:, name:, language:, code_html: }]; the order follows
    # a hash of the task and the address, so it is the same at each visit
    # and not the order of the names.
    def self.html(task_key, number, title, entries)
        return '' if entries.empty?

        ordered = entries.sort_by { |e| Digest::SHA256.hexdigest("#{task_key}\n#{e[:email]}") }
        io = +"<details class='class-answers kata-hand-ins'><summary>#{number}. #{h(title)}: "
        io << "#{entries.size} #{entries.size == 1 ? 'Lösung' : 'Lösungen'} eingereicht</summary>"
        io << "<p class='kata-hand-ins-note'>#{h(NOTE)}</p>"
        io << "<label class='kata-hand-ins-names'><input type='checkbox' class='form-check-input'> Namen zeigen</label>"
        io << '<ol class="kata-hand-ins-list">'
        ordered.each_with_index do |e, i|
            io << "<li><button type='button' class='btn btn-link kata-hand-in' data-label='Lösung #{i + 1}' data-name='#{h(e[:name])}'>"
            io << "Lösung #{i + 1}</button> <span class='kata-hand-in-name' hidden>#{h(e[:name])}</span>"
            io << "<template><pre class='kata-hand-in-code code-#{h(e[:language])}'>#{e[:code_html]}</pre></template></li>"
        end
        io << '</ol></details>'
    end
end
