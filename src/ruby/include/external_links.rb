# Links to other sites open in a new tab (an RFC, a paper, the official
# documentation), so the page with the activity stays where the student
# was. Used for the pages themselves, the task cards on them and the texts
# a check sends back (TaskHtml).
module ExternalLinks
    EXTERNAL = %r{\Ahttps?://}i

    def self.mark(root)
        root.css('a[href]').each do |a|
            next unless a['href'].to_s.strip.match?(EXTERNAL)

            a['target'] = '_blank'
            a['rel'] = 'noopener'
        end
        root
    end
end
