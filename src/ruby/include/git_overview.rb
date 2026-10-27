require 'cgi'
require_relative 'git_server'

# The page /git: every repository on the Workspace's Git server in the
# groups a teacher teaches (an administrator: in all groups, with their
# teachers; a student: in their own groups), one table, with its address,
# how the group's students get their keys, the last commit and the way to
# look at it and to the group's settings; a repository of an activity
# (class repository) or a free one a teacher made here. Below, the person's
# own repositories for practice, and for an administrator that such exist
# (docs/teacher-access.md, »Repositorys im Workspace«). Plain Ruby, tested
# on its own (test/git_overview_test.rb); main.rb decides whose groups and
# passes what it knows.
module GitOverview
    KEY_LABELS = {
        GitServer::KEYS_AUTO => 'automatisch',
        GitServer::KEYS_HANDED_IN => 'abgegeben',
    }.freeze

    # groups: the groups the person may see, in their order.
    # git_groups: { group => { id:, keys: } } (Main.git_groups).
    # repos: ->(git_id) { [{ name:, size:, last:, author: }] } (GitServer.repos).
    # titles: ->(slug) { title or nil }: the activity a repository belongs
    # to (a class repository); nil for a free one.
    # class_repos: { group => { slug => address } }, to tell whether a
    # repository is the group's class repository of its page.
    # teachers: { group => [names] }, for an administrator; nil otherwise.
    # Returns [{ group:, id:, name:, title:, free:, description:, address:, keys:, last:, author:, size:, in_use:, teachers: }].
    def self.rows(groups:, git_groups:, repos:, titles:, class_repos: {}, teachers: nil)
        groups.flat_map do |group|
            data = git_groups[group]
            next [] unless data && data[:id]

            repos.call(data[:id]).map do |repo|
                address = GitServer.address(data[:id], repo[:name])
                title = titles.call(repo[:name])
                { :group => group, :id => data[:id], :name => repo[:name], :title => title, :free => title.nil?,
                  :description => repo[:description].to_s, :address => address, :keys => data[:keys] || GitServer::KEYS_HANDED_IN,
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

    # The address, breaking after the colon only (default.css).
    def self.address_html(address)
        "<code>#{h(address).split(':', 2).map { |part| "<span>#{part}</span>" }.join(':<wbr>')}</code>"
    end

    # The table, or what to do while there is none. student: a student's
    # view, the repositories of their groups to look at, nothing to set.
    def self.html(rows, admin: false, student: false, web_root: '')
        intro = if admin then 'Alle Repositorys auf dem Git-Server des Workspace, in allen Gruppen.'
                elsif student then 'Die Repositorys deiner Gruppen auf dem Git-Server des Workspace.'
                else 'Die Repositorys auf dem Git-Server des Workspace in den Gruppen, die du unterrichtest.'
                end
        io = +"<p class='git-overview-intro'>#{intro} Erreichbar sind sie nur aus den Workspaces der Gruppe; " \
            'hier im Browser kannst du sie ansehen.</p>'
        if rows.empty?
            return io << "<p class='git-overview-empty'><i class='bi bi-git' aria-hidden='true'></i>Deine Gruppen haben noch keine Repositorys im Workspace.</p>" if student

            return io << "<p class='git-overview-empty'><i class='bi bi-git' aria-hidden='true'></i>Noch keine Repositorys. " \
                "Das Repository einer Aktivität legst du auf der <a href='#{h(web_root)}/klasse?tab=einstellungen'>Klassenseite</a> an: " \
                'im Reiter »Einstellungen« unter »Git-Repositorys« mit »Im Workspace anlegen«; eins ohne Aktivität mit »Neues Repository«.</p>'
        end

        io << "<div class='git-overview-scroll'><table class='table git-overview'><thead><tr><th>Gruppe</th><th>Repository</th>" \
            "<th>Adresse</th>#{student ? '' : '<th>Schlüssel</th>'}<th>Zuletzt</th>#{admin ? '<th>Lehrkräfte</th>' : ''}<th></th></tr></thead><tbody>"
        rows.each do |r|
            title = if r[:free]
                        "<strong>#{h(r[:name])}</strong><span class='git-overview-free' title='Ein Repository der Gruppe ohne Aktivität'>frei</span>"
                    else
                        "<a href='#{h(web_root)}/#{h(r[:name])}'>#{h(r[:title])}</a>"
                    end
            title += "<span class='git-overview-description'>#{h(r[:description])}</span>" unless r[:description].to_s.empty?
            unused = r[:in_use] || r[:free] ? '' : "<span class='git-overview-unused' title='Die Gruppe hat für diese Seite eine andere Adresse eingetragen oder keine.'>nicht eingetragen</span>"
            last = r[:last] ? "<span class='git-overview-time'>#{h(time(r[:last]))}</span><span class='git-overview-author'>#{h(r[:author])}</span>" : '<span class="git-overview-none">noch kein Commit</span>'
            settings = "#{web_root}/klasse?group=#{CGI.escape(r[:group])}&repos=#{CGI.escape(r[:name])}"
            # data-label: the column's name, shown above each value on a phone (default.css)
            io << "<tr><td data-label='Gruppe'>#{h(r[:group])}</td><td data-label='Repository'>#{title}#{unused}</td>" \
                "<td data-label='Adresse'>#{address_html(r[:address])}</td>"
            io << "<td data-label='Schlüssel'>#{h(KEY_LABELS[r[:keys]] || r[:keys])}</td>" unless student
            io << "<td data-label='Zuletzt'>#{last}</td>"
            io << "<td data-label='Lehrkräfte'>#{(r[:teachers] || []).map { |t| h(t) }.join(', ')}</td>" if admin
            io << "<td><div class='git-overview-actions'><a class='btn btn-sm btn-outline-secondary' href='#{h(web_root)}/git/#{h(r[:id])}/#{h(r[:name])}'>" \
                "<i class='bi bi-eye' aria-hidden='true'></i>Ansehen</a>"
            if student
                # nothing to set
            elsif r[:free]
                # a free repository goes here; one of an activity on the class page
                io << "<button type='button' class='btn btn-sm btn-outline-danger' data-git-delete='group' data-group='#{h(r[:group])}' data-name='#{h(r[:name])}'>" \
                    "<i class='bi bi-trash' aria-hidden='true'></i>Löschen</button>"
            else
                io << "<a class='btn btn-sm btn-outline-secondary' href='#{h(settings)}'><i class='bi bi-sliders' aria-hidden='true'></i>Einstellungen</a>"
            end
            io << '</div></td></tr>'
        end
        io << '</tbody></table></div>'
    end

    # The form for a new repository: in one of groups (a teacher's), or the
    # person's own (groups nil). Name, description, a README.md to start.
    def self.new_form_html(groups: nil, disabled: nil)
        kind = groups ? 'group' : 'mine'
        io = +"<div class='git-new' data-git-new='#{kind}'>"
        io << "<button type='button' class='btn btn-success git-new-open' aria-expanded='false'#{disabled ? " disabled title='#{h(disabled)}'" : ''}>" \
            "<i class='bi bi-plus-lg' aria-hidden='true'></i>Neues Repository</button>"
        io << "<form class='git-new-form' hidden novalidate>"
        if groups
            io << "<div class='git-new-field'><label class='form-label' for='git-new-group'>Gruppe</label>" \
                "<select class='form-select' id='git-new-group' name='group'>#{groups.map { |g| "<option value='#{h(g)}'>#{h(g)}</option>" }.join}</select>" \
                "<div class='form-text'>Mitglieder und Lehrkräfte der Gruppe dürfen es klonen und pushen, mit den Schlüsseln der Gruppe.</div></div>"
        end
        io << "<div class='git-new-field'><label class='form-label' for='git-new-name-#{kind}'>Name</label>" \
            "<input class='form-control git-new-name' id='git-new-name-#{kind}' name='name' type='text' maxlength='63' autocomplete='off' spellcheck='false' " \
            "placeholder='zum-beispiel-uebung-1' aria-describedby='git-new-name-#{kind}-help'>" \
            "<div class='form-text' id='git-new-name-#{kind}-help'>Kleinbuchstaben, Ziffern und Bindestriche; der Name steht in der Adresse.</div>" \
            "<div class='invalid-feedback'></div></div>"
        io << "<div class='git-new-field'><label class='form-label' for='git-new-description-#{kind}'>Beschreibung <span class='git-new-optional'>(freiwillig)</span></label>" \
            "<input class='form-control' id='git-new-description-#{kind}' name='description' type='text' maxlength='#{GitServer::DESCRIPTION_MAX}'></div>"
        io << "<div class='form-check git-new-field'><input class='form-check-input' type='checkbox' id='git-new-readme-#{kind}' name='readme' checked>" \
            "<label class='form-check-label' for='git-new-readme-#{kind}'>Mit einer Datei <code>README.md</code> beginnen</label>" \
            "<div class='form-text'>Ohne sie ist das Repository leer; den ersten Commit pushst du dann selbst.</div></div>"
        io << "<div class='git-new-buttons'><button type='submit' class='btn btn-success'><i class='bi bi-check-lg' aria-hidden='true'></i>Anlegen</button>" \
            "<button type='button' class='btn btn-outline-secondary git-new-cancel'>Abbrechen</button></div>"
        io << '</form></div>'
    end

    # The person's own repositories: repos as GitServer.repos gives them,
    # id the person's folder (nil while they have none).
    def self.personal_html(repos, id:, web_root: '')
        io = +''
        if repos.empty?
            return io << "<p class='git-overview-empty'><i class='bi bi-journal-code' aria-hidden='true'></i>Noch keine; " \
                'mit »Neues Repository« legst du eins an.</p>'
        end

        io << "<div class='git-overview-scroll'><table class='table git-overview git-overview-mine'><thead><tr><th>Repository</th><th>Adresse</th>" \
            '<th>Zuletzt</th><th>Größe</th><th></th></tr></thead><tbody>'
        repos.each do |r|
            name = "<strong>#{h(r[:name])}</strong>"
            name += "<span class='git-overview-description'>#{h(r[:description])}</span>" unless r[:description].to_s.empty?
            last = r[:last] ? "<span class='git-overview-time'>#{h(time(r[:last]))}</span>" : '<span class="git-overview-none">noch kein Commit</span>'
            io << "<tr><td data-label='Repository'>#{name}</td><td data-label='Adresse'>#{address_html(GitServer.address(id, r[:name]))}</td>" \
                "<td data-label='Zuletzt'>#{last}</td><td data-label='Größe'>#{h(size(r[:size]))}</td>" \
                "<td><div class='git-overview-actions'><a class='btn btn-sm btn-outline-secondary' href='#{h(web_root)}/git/#{h(id)}/#{h(r[:name])}'>" \
                "<i class='bi bi-eye' aria-hidden='true'></i>Ansehen</a>" \
                "<button type='button' class='btn btn-sm btn-outline-danger' data-git-delete='mine' data-name='#{h(r[:name])}'>" \
                "<i class='bi bi-trash' aria-hidden='true'></i>Löschen</button></div></td></tr>"
        end
        io << '</tbody></table></div>'
    end

    # For an administrator: that people keep repositories to practise, whose,
    # how big and when last changed, and a way to delete one; no way to look
    # into them. rows: [{ owner:, id:, name:, size:, last: }].
    def self.personal_admin_html(rows)
        return "<p class='git-overview-empty'>Niemand hat eigene Repositorys.</p>" if rows.empty?

        io = +"<div class='git-overview-scroll'><table class='table git-overview'><thead><tr><th>Person</th><th>Repository</th>" \
            '<th>Zuletzt</th><th>Größe</th><th></th></tr></thead><tbody>'
        rows.each do |r|
            last = r[:last] ? "<span class='git-overview-time'>#{h(time(r[:last]))}</span>" : '<span class="git-overview-none">noch kein Commit</span>'
            io << "<tr><td data-label='Person'>#{h(r[:owner])}</td><td data-label='Repository'><code>#{h(r[:name])}</code></td>" \
                "<td data-label='Zuletzt'>#{last}</td><td data-label='Größe'>#{h(size(r[:size]))}</td>" \
                "<td><div class='git-overview-actions'><button type='button' class='btn btn-sm btn-outline-danger' data-git-delete='personal' " \
                "data-id='#{h(r[:id])}' data-name='#{h(r[:name])}'><i class='bi bi-trash' aria-hidden='true'></i>Löschen</button></div></td></tr>"
        end
        io << '</tbody></table></div>'
    end

    def self.size(bytes)
        return "#{bytes} Bytes" if bytes < 1024
        return format('%.0f KB', bytes / 1024.0) if bytes < 1024 * 1024

        format('%.1f MB', bytes / 1024.0 / 1024.0).tr('.', ',')
    end
end
