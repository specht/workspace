require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/content_parts'

class ContentPartsTest < Minitest::Test
    CONTENT = File.expand_path('../../content', __dir__)

    def setup
        @root = Dir.mktmpdir
        @reference = File.join(@root, 'reference')
        FileUtils.mkdir_p(File.join(@reference, 'workspace'))
        File.write(File.join(@reference, 'workspace', 'workspace.md'), <<~MD)
            # Workspace

            <!-- part: klonen -->
            <!-- short: Klone `${template}` & öffne es. -->
            ## Repository klonen

            Klone `${template}`.

            <img class='full' src='git-clone.webp' alt=''>
            <!-- /part -->

            Zwischentext.

            <!-- part: terminal -->
            ## Terminal öffnen

            Drücke <kbd>Strg</kbd><kbd>J</kbd>.
        MD
        @page = File.join(@root, 'showerjs')
        FileUtils.mkdir_p(@page)
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    # An HTML block that Redcarpet cannot close (a </div> that does not start
    # a line, as in </table></div>) swallows the Markdown after it up to the
    # next such </div>: its headings then appear as raw text.
    def test_every_handbuch_heading_is_rendered
        require 'redcarpet'
        redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, { :fenced_code_blocks => true })
        Dir[File.join(CONTENT, 'reference', '*', '*.md')].each do |file|
            next if File.basename(file) == ContentParts::NOTE_FILE
            markdown = ContentParts.fill_variables(ContentParts.mark_parts(File.read(file, encoding: 'UTF-8')))
            fence = false
            # Screenshot recipes are comments with # lines of their own.
            headings = markdown.gsub(/<!--.*?-->/m, '').lines.count do |line|
                fence = !fence if line.start_with?('```')
                !fence && line =~ /\A#+ \S/
            end
            assert_equal headings, redcarpet.render(markdown).scan(/<h[1-6][ >]/).size, file
        end
    end

    def test_page_file_prefers_own_name_and_never_the_teacher_note
        File.write(File.join(@page, 'lehrkraft.md'), 'Notiz')
        File.write(File.join(@page, 'showerjs.md'), 'Seite')
        assert_equal File.join(@page, 'showerjs.md'), ContentParts.page_file(@page, 'showerjs')
        FileUtils.rm(File.join(@page, 'showerjs.md'))
        File.write(File.join(@page, 'zzz.md'), 'andere Seite')
        assert_equal File.join(@page, 'zzz.md'), ContentParts.page_file(@page, 'showerjs')
    end

    def test_every_catalog_entry_resolves_to_its_own_page
        %w[sections.yaml lehrmaterial/sections.yaml reference/sections.yaml].each do |catalog|
            path = File.join(CONTENT, catalog)
            next unless File.exist?(path)
            root = File.dirname(path)
            YAML.safe_load(File.read(path, encoding: 'UTF-8')).each do |section|
                (section['entries'] || []).each do |entry|
                    name = entry.sub(/\A\./, '')
                    file = ContentParts.page_file(File.join(root, name), name)
                    refute_nil file, "#{catalog}: #{name}"
                    refute_equal ContentParts::NOTE_FILE, File.basename(file)
                end
            end
        end
    end

    def test_parts_end_at_end_marker_next_part_or_file_end
        parts = ContentParts.parts(File.read(File.join(@reference, 'workspace', 'workspace.md'), encoding: 'UTF-8'))
        assert_equal %w[klonen terminal], parts.keys
        refute_includes parts['klonen'], 'Zwischentext'
        assert parts['terminal'].end_with?('<kbd>J</kbd>.')
    end

    def test_mark_parts_turns_markers_into_anchors
        marked = ContentParts.mark_parts("<!-- part: klonen -->\n## Klonen\n<!-- /part -->\n")
        assert_includes marked, "<div id='klonen' class='reference-part'></div>"
        refute_includes marked, '/part'
    end

    def test_include_inserts_part_without_heading_with_variables_and_fixed_paths
        warnings = []
        out = ContentParts.expand_includes(
            "Vorher\n\n_include(workspace#klonen)\n\nNachher",
            page_dir: @page, reference_root: @reference,
            variables: { 'template' => 'https://example.org/vorlage.git' },
            warn: ->(m) { warnings << m },
        )
        assert_empty warnings
        refute_includes out, '## Repository klonen'
        assert_includes out, 'Klone `https://example.org/vorlage.git`.'
        assert_includes out, "src='../reference/workspace/git-clone.webp'"
        assert_includes out, "<div class='reference-include-begin'></div>"
        assert_includes out, "data-href='/reference/workspace#klonen'"
        assert_match(/data-label='[^']+ › Repository klonen'/, out)
    end

    # A part's sub-headings come out as h4 whether the part is an h2 or an
    # h3 on its reference page; code blocks are left alone.
    def test_included_sub_headings_are_h4_however_deep_the_part_sits
        File.write(File.join(@reference, 'workspace', 'workspace.md'), <<~MD)
            # Workspace

            <!-- part: flach -->
            ## Flach

            ### Unterpunkt

            ```bash
            # kein Titel
            ```
            <!-- /part -->

            ## Gruppe

            <!-- part: tief -->
            ### Tief

            #### Unterpunkt
            <!-- /part -->
        MD
        %w[flach tief].each do |part|
            out = ContentParts.expand_includes("_include(workspace##{part})", page_dir: @page, reference_root: @reference)
            assert_match(/^#### Unterpunkt$/, out, part)
            refute_match(/^#{'#' * 3} Unterpunkt$|^#{'#' * 5} Unterpunkt$/, out, part)
        end
        out = ContentParts.expand_includes('_include(workspace#flach)', page_dir: @page, reference_root: @reference)
        assert_includes out, "```bash\n# kein Titel\n```"
    end

    def test_include_with_done_names_the_task_that_folds_it
        out = ContentParts.expand_includes('_include(workspace#klonen, done: geklont)', page_dir: @page, reference_root: @reference)
        assert_includes out, "data-title='Repository klonen' data-done='geklont'"
        refute_includes ContentParts.expand_includes('_include(workspace#klonen)', page_dir: @page, reference_root: @reference), 'data-done'
        assert_equal ['geklont'], ContentParts.done_tasks("_include(workspace#klonen, done: geklont)\n_include(workspace#terminal)")
    end

    # The short line stands for the part once a person has been through it
    # elsewhere; it is not part of the text, neither here nor on the
    # reference page.
    def test_short_line_becomes_an_attribute_with_the_page_variables
        out = ContentParts.expand_includes('_include(workspace#klonen)', page_dir: @page, reference_root: @reference,
            variables: { 'template' => 'https://example.org/a.git' })
        assert_includes out, "data-part='workspace#klonen'"
        assert_includes out, "data-short='Klone &lt;code&gt;https://example.org/a.git&lt;/code&gt; &amp;amp; öffne es.'"
        refute_includes out, 'short:'
        refute_includes ContentParts.mark_parts(File.read(File.join(@reference, 'workspace', 'workspace.md'))), 'short:'
        refute_includes ContentParts.expand_includes('_include(workspace#terminal)', page_dir: @page, reference_root: @reference), 'data-short'
    end

    def test_included_parts_lists_each_part_once
        assert_equal ['workspace#klonen', 'workspace#terminal'],
            ContentParts.included_parts("_include(workspace#klonen, done: x)\n_include(workspace#terminal)\n_include(workspace#klonen)")
    end

    # done: must name a task that the same page shows, or nothing ever folds.
    def test_every_done_task_is_on_its_page
        Dir[File.join(CONTENT, '*', '*.md')].each do |file|
            markdown = File.read(file, encoding: 'UTF-8')
            ContentParts.done_tasks(markdown).each do |id|
                assert_match(/^_task\(#{Regexp.escape(id)}\)\s*$/, markdown, "#{file}: done: #{id} has no _task(#{id})")
            end
        end
    end

    def test_missing_part_and_variable_warn_but_do_not_break
        warnings = []
        out = ContentParts.expand_includes('_include(workspace#fehlt) _include(workspace#klonen)',
            page_dir: @page, reference_root: @reference, warn: ->(m) { warnings << m })
        assert_includes out, 'Baustein fehlt: workspace#fehlt'
        assert_includes out, 'Klone `[Adresse der Vorlage]`.'
        assert_equal 2, warnings.size
    end

    def test_only_known_variables_are_filled
        text = 'Klone ${template}; in JavaScript: `${name}` und ${HOME}'
        assert_equal 'Klone https://x.git; in JavaScript: `${name}` und ${HOME}',
            ContentParts.fill_variables(text, 'template' => 'https://x.git')
        assert_equal 'Klone [Adresse der Vorlage]; in JavaScript: `${name}` und ${HOME}',
            ContentParts.fill_variables(text)
    end

    def test_include_file_placeholder_is_left_alone
        text = '_include_file(beispiel.c, c)'
        assert_equal text, ContentParts.expand_includes(text, page_dir: @page, reference_root: @reference)
    end

    def test_uses_links_into_listed_reference_pages
        warnings = []
        out = ContentParts.expand_uses('_uses(terminal, gibtsnicht)', references: ['workspace'],
            reference_root: @reference, warn: ->(m) { warnings << m })
        # The chip shows the part's heading, not its name.
        assert_includes out, "<a href='/reference/workspace#terminal' title='Im Handbuch nachlesen'><i class='bi bi-book' aria-hidden='true'></i>Terminal öffnen</a>"
        assert_includes out, '<span>gibtsnicht</span>'
        assert_equal 1, warnings.size
    end

    def test_meta_is_read_safely
        md = "<div class='meta'>\nkind: projekt\nreference: [workspace]\n</div>\n\n# Titel"
        assert_equal({ 'kind' => 'projekt', 'reference' => ['workspace'] }, ContentParts.meta(md))
        assert_equal({}, ContentParts.meta("<div class='meta'>\n: : kaputt\n</div>"))
        assert_equal({}, ContentParts.meta('# ohne Meta'))
    end
    def test_page_variables
        # class_repo in the meta block only switches the page to a class
        # repository per group (ClassRepos); it is never the address.
        vars = ContentParts.page_variables('template' => ' https://x.git ', 'class_repo' => 'group', 'other' => 'y')
        assert_equal({ 'template' => 'https://x.git' }, vars)
        assert_equal 'git clone [Adresse des Klassen-Repositorys] structures',
            ContentParts.fill_variables('git clone ${class_repo} structures', vars)
        assert_equal 'ssh -T git@git.example.org', ContentParts.fill_variables('ssh -T git@${git_host}', vars)
    end

    def test_condition_blocks_give_two_variants
        page = "A\n<!-- if: class_repo -->\nB\n_task(push)\n<!-- /if -->\n<!-- if: !class_repo -->\nC\n<!-- /if -->\nD\n"
        assert_equal ['class_repo'], ContentParts.conditions(page)
        assert_equal "A\nB\n_task(push)\nD\n", ContentParts.filter_conditions(page, { 'class_repo' => true })
        assert_equal "A\nC\nD\n", ContentParts.filter_conditions(page, { 'class_repo' => false })
        assert_equal "A\nC\nD\n", ContentParts.filter_conditions(page, {})
        assert_empty ContentParts.conditions("A\n<!-- feature: case_server -->\n")
        warnings = []
        warn = ->(w) { warnings << w }
        ContentParts.filter_conditions("<!-- if: teleport -->\nX\n<!-- /if -->\n<!-- /if -->\n", {}, :warn => warn)
        ContentParts.filter_conditions("<!-- if: class_repo -->\n<!-- if: !class_repo -->\nX\n<!-- /if -->\n", {}, :warn => warn)
        ContentParts.filter_conditions("<!-- if: class_repo -->\nX\n", {}, :warn => warn)
        assert_includes warnings, '<!-- if: teleport -->: unknown condition'
        assert_includes warnings, '<!-- /if --> without <!-- if: … -->'
        assert_includes warnings, '<!-- if: … --> inside <!-- if: class_repo -->: parts do not nest'
        assert_includes warnings, '<!-- if: class_repo --> without <!-- /if -->'
    end

    def test_contact_email_comes_from_the_site
        text = "Schreib an <a href='mailto:${contact_email}'>${contact_email}</a>."
        assert_equal "Schreib an <a href='mailto:a@b.de'>a@b.de</a>.",
                     ContentParts.fill_variables(text, { 'contact_email' => 'a@b.de' })
        assert_includes ContentParts.fill_variables(text, {}), '[Kontaktadresse]'
        refute ContentParts.page_variables({ 'contact_email' => 'x@y.de' }).key?('contact_email')
    end
    # A short line shows `code` and <kbd>keys</kbd>; any other tag outside
    # backticks would appear as text.
    def test_short_lines_have_no_other_html_outside_code
        Dir[File.join(CONTENT, 'reference', '*', '*.md')].each do |file|
            File.read(file).scan(/<!-- short: (.*?) -->/) do |(line)|
                outside = line.gsub(/`[^`]*`/, '').gsub(%r{</?kbd>}, '')
                refute_match(/<\/?[a-zA-Z][^>]*>/, outside, "#{File.basename(file)}: #{line}")
            end
        end
    end

    def test_inline_html_keys_and_code
        assert_equal 'Ordner schließen: <kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>F</kbd>; <code>&lt;kbd&gt;</code> &lt;b&gt;x&lt;/b&gt;',
            ContentParts.inline_html('Ordner schließen: <kbd>Strg</kbd><kbd>K</kbd>, dann <kbd>F</kbd>; `<kbd>` <b>x</b>')
    end

    def test_feature_blocks_stay_only_where_the_feature_is_on
        page = "A\n<!-- feature: case_server -->\nB\n_task(beweis)\n<!-- /feature -->\nC\n"
        assert_equal "A\nB\n_task(beweis)\nC\n", ContentParts.filter_features(page, ['case_server'])
        assert_equal "A\nC\n", ContentParts.filter_features(page, [])
        warnings = []
        ContentParts.filter_features("<!-- feature: teleport -->\nX\n", [], :warn => ->(w) { warnings << w })
        assert_includes warnings, '<!-- feature: teleport -->: unknown feature'
        assert_includes warnings, '<!-- feature: teleport --> without <!-- /feature -->'
    end

    def test_paths_in_code_examples_stay_as_written
        text = "<img src='bild.webp' alt=''>\n\n```html\n<img src=\"pictures/fuji.jpg\" alt=\"\">\n```\n\n" \
               "<pre><code>&lt;img src=\"Fuji.jpg\"&gt;</code></pre>\n\nIm Text: `src=\"fuji.jpg\"`, und ![Bild](foto.webp)\n"
        out = ContentParts.rewrite_relative_sources(text, from_dir: '/c/reference/html', to_dir: '/c/showerjs')
        assert_includes out, "<img src='../reference/html/bild.webp'"
        assert_includes out, '<img src="pictures/fuji.jpg" alt="">'
        assert_includes out, '&lt;img src="Fuji.jpg"&gt;'
        assert_includes out, '`src="fuji.jpg"`'
        assert_includes out, '![Bild](../reference/html/foto.webp)'
    end
end
