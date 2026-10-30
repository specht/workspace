require 'minitest/autorun'
require_relative '../include/git_overview'

class GitOverviewTest < Minitest::Test
    GIT_GROUPS = {
        '9b' => { :id => 'a1b2c3d4e5', :keys => 'auto' },
        '10a' => { :id => 'f6g7h8i9j0', :keys => 'handed_in' },
        'other' => { :id => 'k1l2m3n4o5', :keys => 'auto' },
    }.freeze
    LAST = Time.at(1_790_000_000)
    REPOS = {
        'a1b2c3d4e5' => [{ :name => 'class-quiz', :size => 2048, :last => LAST, :author => 'Ada' },
                         { :name => 'sandbox', :size => 512, :last => nil, :author => nil }],
        'f6g7h8i9j0' => [{ :name => 'class-quiz', :size => 1024, :last => LAST, :author => 'Workspace' }],
        'k1l2m3n4o5' => [{ :name => 'class-quiz', :size => 1, :last => nil, :author => nil }],
    }.freeze

    def rows(groups, teachers: nil, class_repos: {})
        GitOverview.rows(:groups => groups, :git_groups => GIT_GROUPS, :repos => ->(id) { REPOS[id] || [] },
            :titles => ->(slug) { slug == 'class-quiz' ? 'Ein Quiz für die Klasse' : nil },
            :class_repos => class_repos, :teachers => teachers)
    end

    # Only the groups given (the ones the teacher teaches), in their order,
    # and only groups that have a folder on the server
    def test_only_the_teachers_groups
        list = rows(%w[10a 9b nofolder])
        assert_equal [%w[10a class-quiz], %w[9b class-quiz], %w[9b sandbox]], list.map { |r| [r[:group], r[:name]] }
        refute(list.any? { |r| r[:group] == 'other' })
        assert_equal 'git@git.workspace:a1b2c3d4e5/class-quiz.git', list[1][:address]
        assert_nil list[2][:title], 'a repository without a page of that name keeps its name'
    end

    # A repository is »in use« when the group's class repository of its
    # page is this one's address; one the teacher replaced is marked
    def test_in_use_and_unused
        list = rows(%w[9b], :class_repos => { '9b' => { 'class-quiz' => 'git@git.workspace:a1b2c3d4e5/class-quiz.git' } })
        assert list[0][:in_use]
        refute list[1][:in_use]
        assert_equal 0, GitOverview.html(list).scan('nicht eingetragen').size, 'a free repository is no activity\'s'
        assert_equal 1, GitOverview.html(rows(%w[9b])).scan('nicht eingetragen').size
    end

    # A repository without an activity of its name is a free one: its
    # description, deleted here instead of on the class page.
    def test_a_free_repository
        list = GitOverview.rows(:groups => %w[9b], :git_groups => GIT_GROUPS,
            :repos => ->(_) { [{ :name => 'projekt', :size => 1, :last => nil, :author => nil, :description => 'Unser <Projekt>' }] },
            :titles => ->(_) { nil })
        assert list[0][:free]
        html = GitOverview.html(list)
        assert_includes html, '>frei</span>'
        assert_includes html, 'Unser &lt;Projekt&gt;'
        assert_includes html, "data-git-delete='group' data-group='9b' data-name='projekt'"
        refute_includes html, 'Einstellungen'
    end

    # A student sees the groups' repositories to look at, nothing to set or delete.
    def test_a_student_looks_only
        html = GitOverview.html(rows(%w[9b]), :student => true)
        assert_includes html, 'deiner Gruppen'
        assert_includes html, "href='/git/a1b2c3d4e5/class-quiz'"
        refute_includes html, 'Einstellungen'
        refute_includes html, 'data-git-delete'
        refute_includes html, '<th>Schlüssel</th>'
        assert_includes GitOverview.html([], :student => true), 'noch keine Repositorys'
    end

    # One's own repositories with their address; for an administrator only
    # that they exist, with no way to look into them.
    def test_own_repositories
        mine = [{ :name => 'uebung', :size => 2048, :last => LAST, :author => 'Ada', :description => 'Branches' }]
        html = GitOverview.personal_html(mine, :id => 'p1p1p1p1p1')
        assert_includes html, 'p1p1p1p1p1/uebung.git'
        assert_includes html, "href='/git/p1p1p1p1p1/uebung'"
        assert_includes html, "data-git-delete='mine' data-name='uebung'"
        assert_includes GitOverview.personal_html([], :id => nil), 'Noch keine'
        admin = GitOverview.personal_admin_html([{ :owner => 'Schülerin A', :id => 'p1p1p1p1p1', :name => 'uebung', :size => 2048, :last => LAST }])
        assert_includes admin, 'Schülerin A'
        assert_includes admin, "data-git-delete='personal' data-id='p1p1p1p1p1'"
        refute_includes admin, '/git/p1p1p1p1p1', 'no way into it'
    end

    def test_the_form_for_a_new_one
        form = GitOverview.new_form_html(:groups => ['9b', '<10a>'])
        assert_includes form, "data-git-new='group'"
        assert_includes form, "<option value='&lt;10a&gt;'>"
        mine = GitOverview.new_form_html(:disabled => 'Du hast schon 10 Repositorys.')
        assert_includes mine, "data-git-new='mine'"
        assert_includes mine, 'disabled'
    end

    def test_the_table
        html = GitOverview.html(rows(%w[9b]))
        assert_includes html, "<a href='/class-quiz'>Ein Quiz für die Klasse</a>"
        assert_includes html, '<code><span>git@git.workspace</span>:<wbr><span>a1b2c3d4e5/class-quiz.git</span></code>'
        assert_includes html, "href='/git/a1b2c3d4e5/class-quiz'"
        assert_includes html, "href='/klasse?group=9b&amp;repos=class-quiz'"
        assert_includes html, '<span class=\'git-overview-author\'>Ada</span>'
        assert_includes html, 'noch kein Commit'
        refute_includes html, '<th>Schlüssel</th>', 'how keys come is no choice any more'
        refute_includes html, 'Lehrkräfte', 'teachers only for an administrator'
        refute_match(/[\w.+-]+@(?!git\.workspace)/, html, 'no e-mail address in the table, only the server\'s')
    end

    def test_an_administrator_sees_the_teachers
        html = GitOverview.html(rows(%w[9b 10a], :teachers => { '9b' => ['Lehrkraft A'], '10a' => [] }), :admin => true)
        assert_includes html, '<th>Lehrkräfte</th>'
        assert_includes html, ">Lehrkraft A</td>"
        assert_includes html, 'in allen Gruppen'
    end

    def test_names_are_escaped
        list = GitOverview.rows(:groups => ['<b>'], :git_groups => { '<b>' => { :id => 'a1b2c3d4e5', :keys => 'auto' } },
            :repos => ->(_) { [{ :name => 'class-quiz', :size => 1, :last => LAST, :author => '<script>' }] },
            :titles => ->(_) { 'A & B' })
        html = GitOverview.html(list)
        refute_includes html, '<script>'
        refute_includes html, '<b>'
        assert_includes html, 'A &amp; B'
        assert_includes html, 'group=%3Cb%3E'
    end

    def test_the_way_to_a_first_one
        html = GitOverview.html([])
        assert_includes html, 'Noch keine Repositorys'
        assert_includes html, '»Im Workspace anlegen«'
        refute_includes html, '<table'
    end
end
