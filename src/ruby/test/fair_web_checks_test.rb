require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/file_checks'

# The checks of »Fair, barrierefrei, sicher« (fair-web) against its two ends:
# the starter (src/static/fair-web), where every one finds its barrier, and
# the page as it ends (src/content/fair-web/stufen), where none does.
class FairWebChecksTest < Minitest::Test
    STARTER = File.expand_path('../../static/fair-web', __dir__)
    DONE = File.expand_path('../../content/fair-web/stufen', __dir__)

    CHECKS = {
        'lang' => ['html', { 'elements' => [{ 'selector' => 'html', 'attr' => 'lang' }] }, :missing_element],
        'alt' => ['html', { 'img' => { 'alt' => 'descriptive' } }, :alt_file_name],
        'labels' => ['html', { 'labels' => 'required' }, :label_missing],
        'no clickable div' => ['html', { 'elements' => [{ 'selector' => 'div', 'attr' => 'onclick', 'max' => 0 }] }, :too_many_elements],
        'names' => ['html', { 'names' => 'required' }, nil],
        'headings' => ['html', { 'headings' => 'in_order' }, :heading_skipped],
        'main and nav' => ['html', { 'elements' => [{ 'selector' => 'main', 'min' => 1 }, { 'selector' => 'nav', 'min' => 1 }] }, :missing_element],
        'no box ticked' => ['html', { 'elements' => [{ 'selector' => 'input', 'attr' => 'checked', 'max' => 0 }] }, :too_many_elements],
        'contrast' => ['css', { 'contrast' => { 'min' => 4.5 } }, :low_contrast],
        'focus' => ['css', { 'declares' => [{ 'selector' => ':focus-visible', 'property' => 'outline' }] }, :missing_declaration],
        'motion' => ['css', { 'declares' => [{ 'media' => 'prefers-reduced-motion', 'property' => 'animation' }] }, :missing_declaration],
    }.freeze

    def workspace(from)
        root = Dir.mktmpdir
        FileUtils.mkdir_p(File.join(root, 'fair-web'))
        %w[index.html style.css].each { |f| FileUtils.cp(File.join(from, f), File.join(root, 'fair-web', f)) }
        [root, WorkspaceFiles.new(root)]
    end

    def run_check(ws, type, expect)
        file = type == 'html' ? 'fair-web/index.html' : 'fair-web/style.css'
        FileChecks.send(type, { 'type' => type, 'file' => file, 'expect' => expect }, ws)
    end

    def test_the_starter_has_every_barrier
        root, ws = workspace(STARTER)
        CHECKS.each do |name, (type, expect, problem)|
            next if problem.nil?

            outcome = run_check(ws, type, expect)
            refute outcome.ok, "#{name}: the starter passes"
            assert_equal problem, outcome.problem, "#{name}: #{outcome.message}"
        end
        # the arrow »↑« has a name (its text), but the icon-only top link is a div;
        # its name is checked once it is a link or a button
    ensure
        FileUtils.rm_rf(root) if root
    end

    def test_the_finished_page_passes_every_check
        root, ws = workspace(DONE)
        CHECKS.each do |name, (type, expect, _)|
            outcome = run_check(ws, type, expect)
            assert outcome.ok, "#{name}: #{outcome.message}"
        end
    ensure
        FileUtils.rm_rf(root) if root
    end
end
