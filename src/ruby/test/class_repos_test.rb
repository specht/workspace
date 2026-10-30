require 'minitest/autorun'
require 'yaml'
require_relative '../include/class_repos'
require_relative '../include/content_parts'
require_relative '../include/task_card'
require_relative '../include/student_home'

# The class repository per group (ClassRepos): which addresses a teacher may
# enter, how they are stored, which address and which variant of a page a
# person sees, and which tasks count for them.
class ClassReposTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)
    SANDBOXES = {
        'sandbox-sortieren' => %w[fehlstaende rennen-sortiert verdoppeln stabilitaet],
        'sandbox-datenstrukturen' => %w[lifo-fifo einfuegen-verschieben avl-rotation buckets-belegt],
        'sandbox-cpu' => %w[schleife-verdoppeln ueberlauf takte-schleife],
    }.freeze
    GIT_TASKS = %w[ssh-schluessel git-name klassen-repository nur-dein-ordner veroeffentlicht].freeze

    def test_accepts_the_three_forms
        [
            'git@git.example.org:lehrkraft/klasse-algorithmen.git',
            'git@git.example.org:lehrkraft/klasse',
            'gitea@code.schule.de:informatik/10b/structures.git',
            'ssh://git@git.example.org/lehrkraft/klasse.git',
            'ssh://git.example.org:2222/lehrkraft/klasse.git',
            'https://git.example.org/lehrkraft/klasse.git',
            'https://gitlab.com/lehrkraft/klasse',
            'https://git.example.org:8443/lehrkraft/klasse.git',
            "  git@git.example.org:lehrkraft/klasse.git \n",
        ].each { |address| assert ClassRepos.valid?(address), address }
        assert_equal 'git@git.example.org:a/b.git', ClassRepos.normalize("  git@git.example.org:a/b.git\n")
    end

    # It ends up in a shell command the students copy: nothing but a plain
    # address gets through.
    def test_rejects_everything_else
        [
            '', ' ', nil,
            'http://git.example.org/a/b.git',
            'https://user:secret@git.example.org/a/b.git',
            'https://git.example.org/a b.git',
            'git@git.example.org:a/b.git; rm -rf ~',
            'git@git.example.org:a/b.git && echo',
            'git@git.example.org:$(whoami).git',
            'git@git.example.org:`id`.git',
            "git@git.example.org:'a'.git",
            'git@git.example.org:"a".git',
            'git@git.example.org:a/b.git|cat',
            'git@git.example.org:a/b.git>x',
            "git@git.example.org:a/b.git\nrm",
            'git@git.example.org:../etc/passwd',
            'git@git.example.org:a/../b.git',
            'git@git.example.org:/srv/git/a.git',
            'git@-oProxyCommand=evil:a.git',
            'git@git.example.org:-u.git',
            'git@git_example.org:a.git',
            'git@.example.org:a.git',
            'git@git.example.org:',
            'ext::sh -c touch% /tmp/x',
            'file:///srv/git/a.git',
            '/srv/git/a.git',
            '--upload-pack=touch /tmp/x',
            'ssh://git.example.org:99999999/a.git',
            'ssh://git.example.org:70000/a.git',
            'https://git.example.org:0/a.git',
            'https://git.example.org/',
            'https://git.example.org/a/b.git?x=1',
            'https://git.example.org/a/b.git#x',
            "git@git.example.org:#{'a' * 300}.git",
            'git@git.example.org:ä/b.git',
        ].each { |address| refute ClassRepos.valid?(address), address.inspect }
    end

    def test_host_and_values
        assert_equal 'git.example.org', ClassRepos.host('git@Git.Example.org:a/b.git')
        assert_equal 'git.example.org', ClassRepos.host('ssh://git@git.example.org:2222/a/b.git')
        assert_equal 'gitlab.com', ClassRepos.host('https://gitlab.com/a/b')
        assert_nil ClassRepos.host('nonsense')
        values = ClassRepos.values('git@code.schule.de:lk/algorithms.git')
        assert_equal({ 'class_repo' => 'git@code.schule.de:lk/algorithms.git', 'git_host' => 'code.schule.de',
                       'git_web' => 'https://code.schule.de/', 'git_ssh' => 'git@code.schule.de', 'git_server_hint' => '' }, values)
        # ssh -T greets the server as the address's user, on its port.
        assert_equal 'gitea@code.schule.de', ClassRepos.values('gitea@code.schule.de:lk/a.git')['git_ssh']
        assert_equal '-p 2222 git@git.example.org', ClassRepos.values('ssh://git.example.org:2222/a/b.git')['git_ssh']
        assert_equal 'forgejo@git.example.org', ClassRepos.values('ssh://forgejo@git.example.org/a/b.git')['git_ssh']
        assert_equal 'git@gitlab.com', ClassRepos.values('https://gitlab.com/a/b')['git_ssh']
        assert_equal 'https://git.example.org:8443/', ClassRepos.values('https://git.example.org:8443/a/b.git')['git_web']
        assert_equal ClassRepos::PLACEHOLDERS, ClassRepos.values(nil)
        assert_equal ClassRepos::PLACEHOLDERS, ClassRepos.values('kaputt; rm')
    end

    def test_page_meta
        assert ClassRepos.page?('class_repo' => 'group')
        refute ClassRepos.page?('class_repo' => 'git@git.example.org:a/b.git')
        refute ClassRepos.page?({})
        refute ClassRepos.page?(nil)
    end

    def test_stored_as_json_per_group
        json = ClassRepos.update(nil, 'sandbox-cpu', ' git@git.example.org:lk/programs.git ')
        assert_equal({ 'sandbox-cpu' => 'git@git.example.org:lk/programs.git' }, ClassRepos.parse(json))
        json = ClassRepos.update(json, 'sandbox-sortieren', 'https://git.example.org/lk/algorithms.git')
        assert_equal %w[sandbox-cpu sandbox-sortieren], ClassRepos.parse(json).keys
        json = ClassRepos.update(json, 'sandbox-cpu', '')
        assert_equal({ 'sandbox-sortieren' => 'https://git.example.org/lk/algorithms.git' }, ClassRepos.parse(json))
        assert_nil ClassRepos.update(json, 'sandbox-sortieren', nil)
        # Invalid entries never come back out, whatever is stored.
        assert_equal({}, ClassRepos.parse('{"sandbox-cpu": "a; rm -rf ~", "../x": "git@h:a.git"}'))
        assert_equal({}, ClassRepos.parse('kaputt'))
        assert_equal({}, ClassRepos.parse('[1, 2]'))
        assert_equal({}, ClassRepos.parse(nil))
    end

    def test_the_first_group_with_an_address_counts
        settings = {
            'WP 10' => {},
            '10b' => { 'sandbox-cpu' => 'git@a.example.org:x/cpu.git' },
            'LK' => { 'sandbox-cpu' => 'git@b.example.org:x/cpu.git', 'sandbox-sortieren' => 'git@b.example.org:x/sort.git' },
        }
        groups = ['WP 10', '10b', 'LK']
        assert_equal 'git@a.example.org:x/cpu.git', ClassRepos.for_page('sandbox-cpu', groups, settings)
        assert_equal 'git@b.example.org:x/sort.git', ClassRepos.for_page('sandbox-sortieren', groups, settings)
        assert_nil ClassRepos.for_page('sandbox-datenstrukturen', groups, settings)
        assert_nil ClassRepos.for_page('sandbox-cpu', [], settings)
        assert_equal 'git@a.example.org:x/cpu.git', ClassRepos.first_of(groups, settings)
        assert_nil ClassRepos.first_of(['WP 10'], settings)
    end

    def test_who_sees_which_variant
        settings = { '10b' => { 'sandbox-cpu' => 'git@a.example.org:x/cpu.git' } }
        view = ->(slug, per_group, placeholders, groups) do
            ClassRepos.view(slug, :per_group => per_group, :placeholders => placeholders, :groups => groups, :settings => settings)
        end
        # Teachers, administrators, visitors: with, placeholders.
        assert_equal({ :variant => nil, :address => nil }, view.('sandbox-cpu', true, true, ['10b']))
        # A student whose group has an address: with, that address.
        assert_equal({ :variant => nil, :address => 'git@a.example.org:x/cpu.git' }, view.('sandbox-cpu', true, false, ['10b']))
        # A student whose groups have none for this page: without.
        assert_equal({ :variant => ClassRepos::WITHOUT, :address => nil }, view.('sandbox-sortieren', true, false, ['10b']))
        assert_equal({ :variant => ClassRepos::WITHOUT, :address => nil }, view.('sandbox-cpu', true, false, []))
        # An https:// address: the variant without the SSH key.
        https = { 'LK' => { 'sandbox-cpu' => 'https://git.example.org/x/cpu.git' } }
        assert_equal({ :variant => ClassRepos::HTTPS, :address => 'https://git.example.org/x/cpu.git' },
            ClassRepos.view('sandbox-cpu', :per_group => true, :placeholders => false, :groups => ['LK'], :settings => https))
        # The Handbuch page: the server of the person's class, if any.
        assert_equal({ :variant => nil, :address => 'git@a.example.org:x/cpu.git' }, view.('gitlab', false, false, ['10b']))
        assert_equal({ :variant => nil, :address => nil }, view.('gitlab', false, false, ['WP 10']))
    end

    def test_tokens_are_filled_per_request
        html = "<pre>git clone #{ClassRepos::TOKENS['class_repo']} ~/x</pre><a href=\"#{ClassRepos::TOKENS['git_web']}\">" \
               "#{ClassRepos::TOKENS['git_host']}</a>#{ClassRepos::TOKENS['git_server_hint']}."
        assert_equal '<pre>git clone git@h.example.org:a/b.git ~/x</pre><a href="https://h.example.org/">h.example.org</a>.',
            ClassRepos.fill(html, ClassRepos.values('git@h.example.org:a/b.git'))
        filled = ClassRepos.fill(html, ClassRepos.values(nil))
        assert_includes filled, 'git clone [Adresse des Klassen-Repositorys] ~/x'
        assert_includes filled, '<a href="https://git.example.org/">git.example.org</a> (das ist ein Beispiel'
        assert_equal 'ssh -T git@git.example.org', ClassRepos.neutral("ssh -T git@#{ClassRepos::TOKENS['git_host']}")
        assert_equal 3, ClassRepos.neutral(3)
    end

    # The tokens survive Markdown and the highlighter in one piece: they are
    # letters only.
    def test_tokens_are_plain_words
        ClassRepos::TOKENS.each_value { |token| assert_match(/\A[A-Z]+\z/, token) }
    end

    def test_tasks_count_in_the_variant
        content = {
            :tasks => { :activity => 'sandbox-cpu', :by_id => {}, :on_page => %w[ssh geklont frage push] },
            :variants => { ClassRepos::WITHOUT => { :html => '', :on_page => %w[geklont frage] } },
        }
        assert_equal %w[ssh geklont frage push], ClassRepos.tasks(content, nil)[:on_page]
        assert_equal %w[geklont frage], ClassRepos.tasks(content, ClassRepos::WITHOUT)[:on_page]
        assert_equal 'sandbox-cpu', ClassRepos.tasks(content, ClassRepos::WITHOUT)[:activity]
        # A page without variants is the same for everyone.
        plain = { :tasks => { :on_page => %w[a b] } }
        assert_equal %w[a b], ClassRepos.tasks(plain, ClassRepos::WITHOUT)[:on_page]

        # »x von y Aufgaben« on the start page counts the variant's tasks.
        activity = ->(ids) { [{ :slug => 'sandbox-cpu', :activity => 'sandbox-cpu', :title => 'CPU', :tasks => ids.map { |id| { :id => id, :version => 'v' } } }] }
        states = { 'sandbox-cpu/geklont' => { :status => 'correct', :version => 'v', :updated => 1 } }
        assert_equal 4, StudentHome.progress(activity.(ClassRepos.tasks(content, nil)[:on_page]), states).first[:total]
        assert_equal 2, StudentHome.progress(activity.(ClassRepos.tasks(content, ClassRepos::WITHOUT)[:on_page]), states).first[:total]
    end

    def test_variant_of_an_address
        assert_nil ClassRepos.variant('git@git.example.org:a/b.git')
        assert_nil ClassRepos.variant('ssh://git.example.org:2222/a/b.git')
        assert_equal ClassRepos::HTTPS, ClassRepos.variant('https://git.example.org/a/b.git')
        assert_equal ClassRepos::WITHOUT, ClassRepos.variant(nil)
        assert_equal ClassRepos::WITHOUT, ClassRepos.variant('kaputt; rm')
        assert_equal [nil], ClassRepos.variants([])
        assert_equal [nil, ClassRepos::WITHOUT], ClassRepos.variants(['class_repo'])
        assert_equal [nil, ClassRepos::HTTPS, ClassRepos::WITHOUT], ClassRepos.variants(%w[class_repo class_repo_ssh])
        # Every variant gives every condition a value.
        ClassRepos::CONDITION_VALUES.each_value { |values| assert_equal ClassRepos::CONDITIONS.sort, values.keys.sort }
    end

    # A repository on the Workspace's own server (GitServer): the variant in
    # which the students hand in a key of their own.
    def test_variants_of_the_workspaces_own_server
        local = 'git@git.workspace:a1b2c3d4e5/class-quiz.git'
        mode = ->(address) { address.include?('a1b2c3d4e5') ? true : nil }
        assert_equal ClassRepos::LOCAL_KEYS, ClassRepos.variant(local, :local => mode)
        assert_nil ClassRepos.variant('git@git.example.org:a/b.git', :local => mode)
        assert_nil ClassRepos.variant(local), 'without knowing the server, an SSH address'
        assert_equal [nil, ClassRepos::HTTPS, ClassRepos::LOCAL_KEYS, ClassRepos::WITHOUT],
            ClassRepos.variants(%w[class_repo class_repo_local_keys])
        values = ClassRepos::CONDITION_VALUES
        assert values[ClassRepos::LOCAL_KEYS]['class_repo']
        refute values[ClassRepos::LOCAL_KEYS]['class_repo_ssh'], 'no key for an outside server'
        refute values[ClassRepos::LOCAL_KEYS]['class_repo_outside']
        assert values[nil]['class_repo_outside']
        assert values[ClassRepos::HTTPS]['class_repo_outside']
        assert ClassRepos.valid?(local)
        assert_equal({ :variant => ClassRepos::LOCAL_KEYS, :address => local },
            ClassRepos.view('class-quiz', :per_group => true, :placeholders => false, :groups => ['7b'],
                :settings => { '7b' => { 'class-quiz' => local } }, :local => mode))
    end

    # The sandbox pages themselves: the SSH key only with an SSH address,
    # signing in with a token only with an https:// one, the other Git
    # tasks with either, the questions about the topic and the clone check
    # in all three variants, and every task of tasks.yaml in one of them.
    def test_the_sandbox_pages_in_all_variants
        SANDBOXES.each do |slug, topic_tasks|
            dir = File.join(CONTENT, slug)
            markdown = File.read(File.join(dir, "#{slug}.md"))
            assert ClassRepos.page?(ContentParts.meta(markdown)), "#{slug}: class_repo: group"
            markdown = ContentParts.expand_includes(markdown, :page_dir => dir, :reference_root => File.join(CONTENT, 'reference'))
            assert_equal [nil, ClassRepos::HTTPS, ClassRepos::LOCAL_KEYS, ClassRepos::WITHOUT],
                ClassRepos.variants(ContentParts.conditions(markdown)), slug
            warnings = []
            text = ClassRepos::CONDITION_VALUES.to_h do |variant, values|
                [variant, ContentParts.filter_conditions(markdown, values, :warn => ->(w) { warnings << w })]
            end
            assert_empty warnings, slug
            tasks = text.transform_values { |t| TaskCard.mark_slots(t).last }
            ssh, https, without = tasks.values_at(nil, ClassRepos::HTTPS, ClassRepos::WITHOUT)
            local_keys = tasks[ClassRepos::LOCAL_KEYS]

            ids = YAML.safe_load(File.read(File.join(dir, 'tasks.yaml')))['tasks'].map { |t| t['id'] }
            assert_equal ids.sort, (ssh | https | without).sort, "#{slug}: every task in a variant"
            tasks.each_value { |list| assert_equal list.uniq, list, slug }
            GIT_TASKS.each do |id|
                assert_includes ssh, id, slug
                refute_includes without, id, slug
            end
            (GIT_TASKS - ['ssh-schluessel']).each { |id| assert_includes https, id, slug }
            # The Workspace's own server: a key the student makes and hands in
            GIT_TASKS.each { |id| assert_includes local_keys, id, slug }
            local = text[ClassRepos::LOCAL_KEYS]
            assert_includes local, 'git clone ${class_repo}', slug
            assert_includes local, 'Git-Server des Workspace', slug
            assert_includes local, 'ssh -T git@git.workspace', slug
            refute_includes local, 'lädt dich dorthin ein', slug
            refute_includes local, 'Zugangstoken', slug
            assert_includes local, 'data-git-key', slug
            assert_includes local, 'ssh-keygen', slug
            refute_includes local, 'workspace_git', slug
            refute_includes text[nil], 'git.workspace`. Ein Konto', slug
            refute_includes https, 'ssh-schluessel', slug
            (topic_tasks + ['geklont']).each do |id|
                tasks.each_value { |list| assert_includes list, id, slug }
            end

            assert_includes text[nil], 'git clone ${class_repo}', slug
            assert_includes text[nil], 'ssh -T ', slug
            refute_includes text[nil], 'credential.helper', slug
            assert_includes text[ClassRepos::HTTPS], 'git clone ${class_repo}', slug
            assert_includes text[ClassRepos::HTTPS], 'Zugangstoken', slug
            assert_includes text[ClassRepos::HTTPS], "credential.helper 'cache", slug
            refute_includes text[ClassRepos::HTTPS], 'ssh -T', slug
            refute_includes text[ClassRepos::HTTPS], 'ssh-keygen', slug
            refute_includes text[ClassRepos::WITHOUT], '${class_repo}', slug
            refute_includes text[ClassRepos::WITHOUT], 'git push', slug
            refute_includes text[ClassRepos::WITHOUT], 'Klassen-Repository liegt', slug
            refute_includes text[ClassRepos::WITHOUT], 'ssh-keygen', slug
            folder = text[nil][%r{git clone \$\{class_repo\} ~/[a-z-]+/([a-z]+)}, 1]
            assert_match %r{mkdir ~/[a-z-]+/#{folder}\n}, text[ClassRepos::WITHOUT], slug
            refute_match(/nhcham|GitLab-Kont/, File.read(File.join(dir, "#{slug}.md")) + File.read(File.join(dir, 'lehrkraft.md')) +
                File.read(File.join(dir, 'tasks.yaml')), slug)
        end
    end

    # The Handbuch page names no server of its own; it says what to do on
    # GitLab, Gitea/Forgejo and GitHub.
    def test_the_handbuch_page_is_for_any_git_server
        page = File.read(File.join(CONTENT, 'reference', 'gitlab', 'gitlab.md'))
        refute_match(/nhcham/, page)
        assert_includes page, 'ssh -T ${git_ssh}'
        %w[GitLab Gitea Forgejo GitHub].each { |server| assert_includes page, server }
        assert_match(/<!-- part: https-anmelden -->/, page)
    end

    # The Workspace's own server: under the code block that clones the class
    # repository a button to its page on the site, filled per person.
    def test_a_look_at_the_repository_beside_the_clone_command
        require 'nokogiri'
        token = ClassRepos::TOKENS['class_repo']
        root = Nokogiri::HTML::DocumentFragment.parse("<p>Klonen:</p><pre class='copy-code'>git clone #{token} ~/class-quiz\n</pre><pre>ls</pre>")
        ClassRepos.mark_browse(root)
        buttons = root.css('p.class-repo-browse a')
        assert_equal 1, buttons.size
        assert_equal ClassRepos::BROWSE_TOKEN, buttons.first['href']
        assert_equal 'pre', root.at('p.class-repo-browse').previous_element.name
        assert_includes buttons.first.text, 'im Browser ansehen'
    end
end

