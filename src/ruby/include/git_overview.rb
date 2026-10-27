require 'cgi'
require_relative 'git_server'

# The page /git: every repository on the Workspace's Git server in the
# groups a teacher teaches (an administrator: in all groups, with their
# teachers), one table, with its address, how the group's students get
# their keys, the last commit and the way to look at it and to the group's
# settings (docs/teacher-access.md, »Repositorys im Workspace«). Plain
# Ruby, tested on its own (test/git_overview_test.rb); main.rb decides
# whose groups and passes what it knows.
module GitOverview
    KEY_LABELS = {
        GitServer::KEYS_AUTO => 'automatisch',
        GitServer::KEYS_HANDED_IN => 'abgegeben',
    }.freeze

    # groups: the groups the person may see, in their order.
    # git_groups: { group => { id:, keys: } } (Main.git_groups).
    # repos: ->(git_id) { [{ name:, size:, last:, author: }] } (GitServer.repos).
    # titles: ->(slug) { title or nil }.
    # class_repos: { group => { slug => address } }, to tell whether a
    # repository is the group's class repository of its page.
    # teachers: { group => [names] }, for an administrator; nil otherwise.
    # Returns [{ group:, id:, name:, title:, address:, keys:, last:, author:, size:, in_use:, teachers: }].
    def self.rows(groups:, git_groups:, repos:, titles:, class_repos: {}, teachers: nil)
        groups.flat_map do |group|
            data = git_groups[group]
            next [] unless data && data[:id]

            repos.call(data[:id]).map do |repo|
                address = GitServer.address(data[:id], repo[:name])
                { :group => group, :id => data[:id], :name => repo[:name], :title => titles.call(repo[:name]),
                  :address => address, :keys => data[:keys] || GitServer::KEYS_HANDED_IN,
                  :last => repo[:last], :author => repo[:author], :size => repo[:size],
                  :in_use => (class_repos[group] || {})[repo[:name]] == address,
                  :teachers => teachers && (teachers[group] || []) }
            end
        end
    end

    def self.h(text)
        CGI.escapeHTML(text.to_s)
    end

    def self.time(t)
        t ? t.localtime.strftime('%d.%m.%Y, %H:%M') : nil
    end

    # The table, or what to do while there is none.
    def self.html(rows, admin: false, web_root: '')
        intro = admin ? 'Alle Repositorys auf dem Git-Server des Workspace, in allen Gruppen.' :
            'Die Repositorys auf dem Git-Server des Workspace in den Gruppen, die du unterrichtest.'
        io = +"<p class='git-overview-intro'>#{intro} Erreichbar sind sie nur aus den Workspaces der Gruppe; " \
            'hier im Browser kannst du sie ansehen.</p>'
        if rows.empty?
            return io << "<p class='git-overview-empty'><i class='bi bi-git' aria-hidden='true'></i>Noch keine Repositorys. " \
                "Eins legst du auf der <a href='#{h(web_root)}/klasse?tab=einstellungen'>Klassenseite</a> an: " \
                'im Reiter »Einstellungen« unter »Git-Repositorys« mit »Im Workspace anlegen«.</p>'
        end

        io << "<div class='git-overview-scroll'><table class='table git-overview'><thead><tr><th>Gruppe</th><th>Aktivität</th>" \
            "<th>Adresse</th><th>Schlüssel</th><th>Zuletzt</th>#{admin ? '<th>Lehrkräfte</th>' : ''}<th></th></tr></thead><tbody>"
        rows.each do |r|
            title = r[:title] ? "<a href='#{h(web_root)}/#{h(r[:name])}'>#{h(r[:title])}</a>" : "<code>#{h(r[:name])}</code>"
            unused = r[:in_use] ? '' : "<span class='git-overview-unused' title='Die Gruppe hat für diese Seite eine andere Adresse eingetragen oder keine.'>nicht eingetragen</span>"
            last = r[:last] ? "<span class='git-overview-time'>#{h(time(r[:last]))}</span><span class='git-overview-author'>#{h(r[:author])}</span>" : '<span class="git-overview-none">noch kein Commit</span>'
            settings = "#{web_root}/klasse?group=#{CGI.escape(r[:group])}&repos=#{CGI.escape(r[:name])}"
            # data-label: the column's name, shown above each value on a phone (default.css)
            # the address breaks after the colon only (default.css)
            address = h(r[:address]).split(':', 2).map { |part| "<span>#{part}</span>" }.join(':<wbr>')
            io << "<tr><td data-label='Gruppe'>#{h(r[:group])}</td><td data-label='Aktivität'>#{title}#{unused}</td>" \
                "<td data-label='Adresse'><code>#{address}</code></td>" \
                "<td data-label='Schlüssel'>#{h(KEY_LABELS[r[:keys]] || r[:keys])}</td><td data-label='Zuletzt'>#{last}</td>"
            io << "<td data-label='Lehrkräfte'>#{(r[:teachers] || []).map { |t| h(t) }.join(', ')}</td>" if admin
            io << "<td><div class='git-overview-actions'><a class='btn btn-sm btn-outline-secondary' href='#{h(web_root)}/git/#{h(r[:id])}/#{h(r[:name])}'>" \
                "<i class='bi bi-eye' aria-hidden='true'></i>Ansehen</a>" \
                "<a class='btn btn-sm btn-outline-secondary' href='#{h(settings)}'><i class='bi bi-sliders' aria-hidden='true'></i>Einstellungen</a></div></td></tr>"
        end
        io << '</tbody></table></div>'
    end
end
