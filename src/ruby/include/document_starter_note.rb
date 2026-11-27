require 'nokogiri'

# Under every download of a starting document (a.document-starter, on the
# page or in a task card), for Word or Excel only: a file from the internet
# opens there in »Geschützte Ansicht«, where nothing can be changed until one
# clicks »Bearbeitung aktivieren«. Students who miss the yellow bar think the
# document is broken; LibreOffice and Nextcloud Office have no such view.
# For LibreOffice: which file to hand in when it was saved in its own format.
module DocumentStarterNote
    def self.add(root)
        root.css('a.document-starter').each do |link|
            name = link.at_css('.document-starter-name')&.text.to_s
            program = name =~ /\.xlsx?\z/i ? 'Excel' : name =~ /\.pptx?\z/i ? 'PowerPoint' : 'Word'
            note = Nokogiri::HTML::DocumentFragment.parse(
                "<p class='code-language document-starter-note' data-language='word'>" \
                "<i class='bi bi-shield-exclamation' aria-hidden='true'></i>" \
                "Steht in #{program} oben eine gelbe Leiste <em>Geschützte Ansicht</em>, klick dort auf " \
                "<em>Bearbeitung aktivieren</em>. Erst dann kannst du etwas ändern.</p>")
            # LibreOffice asks on saving which format to keep; the ODF format
            # makes a new file, and whoever then hands in the old one hands in
            # the starting document (the check takes both formats)
            own = name =~ /\.xlsx?\z/i ? ['Excel', '.ods'] : name =~ /\.pptx?\z/i ? ['PowerPoint', '.odp'] : ['Word', '.odt']
            saving = Nokogiri::HTML::DocumentFragment.parse(
                "<p class='code-language document-starter-note' data-language='libreoffice'>" \
                "<i class='bi bi-floppy' aria-hidden='true'></i>" \
                "Fragt LibreOffice beim Speichern nach dem Format, nimm das #{own[0]}-Format. Nimmst du das ODF-Format, " \
                "entsteht eine neue Datei mit der Endung <em>#{own[1]}</em>: Dann lädst du zum Prüfen diese hoch, nicht die alte.</p>")
            # after the paragraph of the link on a page, right after the link in a card
            anchor = link.parent&.name == 'p' && link.parent.children.count(&:element?) == 1 ? link.parent : link
            anchor.add_next_sibling(saving)
            anchor.add_next_sibling(note)
        end
        root
    end
end
