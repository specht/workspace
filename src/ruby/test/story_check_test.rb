require 'minitest/autorun'
require 'fileutils'
require 'json'
require 'tmpdir'
require_relative '../include/task_card'
require_relative '../include/task_check'
require_relative '../include/task_spec'
require_relative '../include/workspace_files'

# The fixture is »Nach Schulschluss« as the tutorial ends, with the analysis
# BIF 9b8abe8 published for it in the screenshot Workspace (the watcher of
# `npm run dev`, which the extension starts): nodes and edges, and no
# scripts, conditions or images in the summary.
class StoryCheckTest < Minitest::Test
    FIXTURE = File.expand_path('fixtures/bif-story', __dir__)

    def setup
        @root = Dir.mktmpdir
        FileUtils.cp_r(FIXTURE, File.join(@root, 'bif'))
        @tasks = TaskSpec.load_file(File.expand_path('../../content/bif/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t] }
    end

    def teardown
        FileUtils.rm_rf(@root)
    end

    def path(rel)
        File.join(@root, 'bif', rel)
    end

    def analysis
        JSON.parse(File.read(path('.story-tools/analysis.json')))
    end

    # Changes the analysis as BIF would after an edit, keeping it current.
    def write_analysis(data)
        data['inputManifest'] = data['inputManifest'].map do |entry|
            { 'path' => entry['path'], 'sha256' => Digest::SHA256.file(path(entry['path'])).hexdigest }
        end
        File.write(path('.story-tools/analysis.json'), JSON.generate(data))
    end

    def run_task(id)
        TaskCheck.check(@tasks.fetch(id), '', :workspace => WorkspaceFiles.new(@root))
    end

    def test_the_tutorial_story_passes_every_tutorial_task
        %w[zweite-seite entscheidung verzweigung lokal schluessel ende].each do |id|
            result = run_task(id)
            assert_equal :correct, result.status, "#{id}: #{result.message}"
        end
        assert_match(/»Nach Schulschluss« in `bif\/pages-starter`: 5 Seiten, 6 Entscheidungen.*1 Ende; 0 Fehler/, run_task('ende').read)
    end

    def node(data, page_id)
        data['nodes'].find { |n| n['pageId'] == page_id }
    end

    def test_scripts_and_conditions_are_counted_from_the_pages
        assert_includes run_task('ende').read, '8 Bedingungen, 2 Skripte'
        data = analysis
        node(data, '3')['counts'].merge!('conditions' => 0, 'scripts' => 0)
        write_analysis(data)
        assert_equal :too_few_scripts, run_task('schluessel').problem
    end

    def test_own_story_tasks_want_another_folder
        result = run_task('eigene-geschichte')
        assert_equal :wrong, result.status
        assert_equal :starter, result.problem
        assert_includes result.message, 'export const path = "pages-meine-geschichte";'
    end

    def test_a_changed_page_makes_the_analysis_stale
        File.write(path('pages-starter/2.md'), "Anders.\n")
        result = run_task('ende')
        assert_equal :wrong, result.status
        assert_equal :stale, result.problem
        assert_includes result.message, '`pages-starter/2.md`'
    end

    def test_missing_analysis_and_missing_project
        FileUtils.rm_rf(path('.story-tools'))
        assert_equal :no_analysis, run_task('ende').problem
        FileUtils.rm_rf(File.join(@root, 'bif'))
        assert_equal :no_project, run_task('ende').problem
    end

    def test_errors_are_named_with_file_line_and_in_german
        data = analysis
        data['summary']['errors'] = 1
        data['diagnostics'] << { 'severity' => 'error', 'code' => 'missing-page', 'file' => 'pages-starter/4.md',
                                 'line' => 3, 'column' => 1, 'message' => "Choice 'x' links to missing page '9'.", 'target' => '9' }
        write_analysis(data)
        result = run_task('ende')
        assert_equal :errors, result.problem
        assert_includes result.message, '`pages-starter/4.md`, Zeile 3: Diese Entscheidung führt zu Seite `9`, aber es gibt keine Datei `9.md`.'
    end

    def test_unreachable_pages_are_no_endings
        data = analysis
        data['summary']['pages'] = 6
        data['nodes'] << { 'kind' => 'page', 'nodeId' => 'node-page-36', 'pageId' => '6', 'path' => 'pages-starter/6.md',
                           'reachable' => false, 'counts' => {} }
        data['diagnostics'] << { 'severity' => 'warning', 'code' => 'unreachable-page', 'file' => 'pages-starter/6.md',
                                 'line' => 1, 'column' => 1, 'message' => "Page '6' is unreachable from page '1'." }
        write_analysis(data)
        assert_equal 1, StoryCheck.endings(analysis).size
        result = run_task('ende')
        assert_equal :unreachable, result.problem
        assert_includes result.message, '`pages-starter/6.md`, Zeile 1: Diese Seite erreicht man von Seite 1 aus nicht.'
    end

    def test_a_story_without_an_ending
        File.write(path('pages-starter/5.md'), "Der Schlüssel passt.\n\n- [Zurück.](2)\n")
        data = analysis
        data['edges'] << { 'sourceNodeId' => node(data, '5')['nodeId'], 'target' => '2' }
        write_analysis(data)
        result = run_task('ende')
        assert_equal :too_few_endings, result.problem
        assert_includes result.message, 'noch kein Ende'
    end

    def test_local_choices_do_not_end_a_page_and_feedback_replaces_counts
        data = analysis
        data['summary']['localChoices'] = 0
        write_analysis(data)
        result = run_task('lokal')
        assert_equal :too_few_local_choices, result.problem
        assert_includes result.message, '`(.)`'
        assert node(analysis, '3')['choices'].any? { |c| c['local'] }
        refute StoryCheck.endings(analysis).map { |n| n['pageId'] }.include?('3')
    end

    def test_own_folder
        FileUtils.mkdir_p(path('pages-meine-geschichte'))
        File.write(path('pages-meine-geschichte/1.md'), "# Meine\n\nAnfang.\n")
        File.write(path('config.js'), "export const path = \"pages-meine-geschichte\";\n")
        data = analysis
        data['project']['pagesPath'] = 'pages-meine-geschichte'
        data['summary'] = data['summary'].merge('pages' => 1, 'pageChoices' => 0)
        data['inputManifest'] = [{ 'path' => 'config.js' }, { 'path' => 'pages-meine-geschichte/1.md' }]
        data['nodes'] = [{ 'kind' => 'page', 'nodeId' => 'node-page-1', 'pageId' => '1', 'path' => 'pages-meine-geschichte/1.md',
                           'reachable' => true, 'counts' => {} }]
        data['edges'] = []
        write_analysis(data)
        assert_equal :correct, run_task('eigener-ordner').status
        result = run_task('eigene-geschichte')
        assert_equal :too_few_pages, result.problem
        assert_equal 'In deiner Geschichte stehen 1 Seite, verlangt sind 5 Seiten.', result.message
    end

    def test_validation
        check = ->(expect) { TaskSpec.validate_check('story', { 'type' => 'story', 'project' => 'bif', 'expect' => expect }) }
        assert_empty check.({ 'pages' => 2, 'own_story' => true })
        refute_empty check.({ 'pages' => 0 })
        refute_empty check.({ 'own_story' => 'ja' })
        refute_empty check.({ 'words' => 3 })
        refute_empty check.({})
    end

    def test_card_names_the_file
        line = TaskCard.reads_line({ 'type' => 'story', 'project' => 'bif' }, ->(s) { s })
        assert_includes line, '/workspace/bif/.story-tools/analysis.json'
    end
end
