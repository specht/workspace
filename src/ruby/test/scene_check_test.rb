require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require 'open3'
require_relative '../include/task_check'

# Runs the real scene_dump.js with node against the viewer's own parser
# (fixtures/anaglyph/include/script.js), like the Workspace would.
class SceneCheckTest < Minitest::Test
    def setup
        skip 'node not installed' unless system('node --version > /dev/null 2>&1')
        @tmp = Dir.mktmpdir
        FileUtils.mkdir_p(File.join(@tmp, 'anaglyph', 'include'))
        FileUtils.cp(File.expand_path('fixtures/anaglyph/include/script.js', __dir__), File.join(@tmp, 'anaglyph', 'include'))
        @ws = WorkspaceFiles.new(@tmp)
        @runner = lambda do |command, _seconds|
            out, = Open3.capture2('sh', '-c', command, chdir: @tmp)
            { :output => out, :status => 0 }
        end
        @tasks = TaskSpec.load_file(File.expand_path('../../content/anaglyph/tasks.yaml', __dir__))['tasks'].to_h { |t| [t['id'], t] }
    end

    def teardown
        FileUtils.rm_rf(@tmp) if @tmp
    end

    def scene(name, text)
        File.write(File.join(@tmp, 'anaglyph', name), text)
    end

    def check(id)
        TaskCheck.check(@tasks.fetch(id), '', variables: {}, workspace: @ws, runner: @runner)
    end

    def test_sphere
        scene('scene.ini', "shape = grid\n\nshape = box\n")
        r = check('kugel')
        assert_equal :wrong, r.status
        assert_includes r.message, 'Ich finde 0 Kugeln'
        assert_includes r.read, 'Zeile 3: Würfel bei (0, 0, 0), Größe 50 × 50 × 50'
        scene('scene.ini', "shape = grid\n\nshape = sphere\n")
        assert_equal :correct, check('kugel').status
    end

    def test_viewer_errors_are_shown
        scene('scene.ini', "shape = kugel\n")
        r = check('kugel')
        assert_equal :scene_error, r.problem
        assert_includes r.message, 'Ungültige Form (shape) in Zeile 1: "kugel"'
    end

    def test_four_boxes_with_feedback
        scene('vier-wuerfel.ini', "shape = box\n\nshape = box\nmove = 100, 0, 0\n\nshape = box\nmove = 0, 100, 0\n\nshape = box\nmove = 0, 0, -100\n")
        r = check('vier-wuerfel')
        assert_equal :wrong, r.status
        assert_equal 'Der Würfel »davor« steht hinten. Vorne ist +z.', r.message
        scene('vier-wuerfel.ini', "shape = box\n\nshape = box\nmove = 100, 0, 0\n\nshape = box\nmove = 0, 100, 0\n\nshape = box\nmove = 0, 0, 100\n")
        assert_equal :correct, check('vier-wuerfel').status
    end

    def test_star
        scene('wuerfelstern.ini', "shape = box\nrotate = 0, 0, -30\n\nshape = box\n\nshape = box\nrotate = 0, 0, 30\n")
        assert_equal :correct, check('wuerfelstern').status
        scene('wuerfelstern.ini', "shape = box\n\nshape = box\n\nshape = box\nrotate = 0, 0, 30\n")
        r = check('wuerfelstern')
        assert_equal :rotation, r.problem
    end

    def test_plank
        scene('latte.ini', "shape = box\nscale = 10, 1, 1\nmove = 0, 25, 0\n")
        assert_equal :correct, check('latte').status, check('latte').message
        scene('latte.ini', "shape = box\nscale = 10, 1, 1\n")
        r = check('latte')
        assert_equal :bottom, r.problem
        assert_includes r.message, 'y = -25'
        assert_includes r.message, 'versinkt'
    end

    def test_lattice_needs_loops
        loops = "loop x from -100 to 100 step 100\nloop y from -100 to 100 step 100\nloop z from -100 to 100 step 100\nshape = box\nmove = x, y, z\n\nend\nend\nend\n"
        scene('wuerfelgitter.ini', loops)
        assert_equal :correct, check('wuerfelgitter').status
        scene('wuerfelgitter.ini', loops.sub('step 100', 'step 200'))
        assert_equal :lattice, check('wuerfelgitter').problem
    end

    def test_solar_system
        good = "shape = sphere\nname = sonne\n\ngroup\nmove = 300, 0, 0\nrotate = 0, t * 30, 0\nshape = sphere\nname = erde\nscale = 0.3, 0.3, 0.3\n\n  group\n  move = 60, 0, 0\n  rotate = 0, t * 90, 0\n  shape = sphere\n  name = mond\n  scale = 0.1, 0.1, 0.1\n  end\nend\n"
        scene('sonnensystem.ini', good)
        r = check('sonnensystem')
        assert_equal :correct, r.status, r.message
        # Earth spinning in place instead of orbiting.
        scene('sonnensystem.ini', good.sub("move = 300, 0, 0\nrotate = 0, t * 30, 0", "rotate = 0, t * 30, 0\nmove = 300, 0, 0"))
        r = check('sonnensystem')
        assert_equal :orbit, r.problem
        assert_includes r.message, '»erde« kreist nicht um »sonne«'
    end

    def test_avenue_with_define
        tree = "define baum\n    shape = cylinder\n    scale = 0.2, 1, 0.2\n    move = 0, 50, 0\n    shape = cone\n    scale = 1.2\n    move = 0, 145, 0\nend\nshape = grid\n"
        rows = "loop z from -200 to 200 step 100\n    shape = baum\n    move = -120, 0, z\n    shape = baum\n    move = 120, 0, z\nend\n"
        scene('allee.ini', tree + rows)
        r = check('allee')
        assert_equal :correct, r.status, r.message
        # A blank line after shape = baum: the left trees all end up in the middle.
        scene('allee.ini', tree + rows.sub("shape = baum\n    move = -120", "shape = baum\n\n    move = -120"))
        r = check('allee')
        assert_equal :rows, r.problem
        assert_includes r.message, 'Leerzeile'
    end

    def test_missing_file_and_viewer
        r = check('allee')
        assert_equal :missing, r.problem
        FileUtils.rm(File.join(@tmp, 'anaglyph', 'include', 'script.js'))
        scene('allee.ini', "shape = box\n")
        assert_equal :no_viewer, check('allee').problem
    end

    def test_gallery_submission_checks_the_scene
        scene('bunt.ini', "shape = box\n")
        r = TaskCheck.check(@tasks['galerie'], 'anaglyph/bunt.ini', variables: {}, workspace: @ws, runner: @runner)
        assert_equal :invalid, r.status
        assert_includes r.message, 'mindestens 5 verlangt'
        scene('bunt.ini', (["shape = box"] * 5).join("\n\n") + "\n")
        r = TaskCheck.check(@tasks['galerie'], 'anaglyph/bunt.ini', variables: {}, workspace: @ws, runner: @runner)
        assert_equal :sent, r.status
        refute_match(/prüft der Workspace noch nicht/, r.warnings.join)
    end

    def test_endless_expression_is_stopped
        scene('scene.ini', "shape = sphere\nmove = (() => { while (true) {} })(), 0, 0\n")
        r = check('kugel')
        refute_equal :correct, r.status
    end
end
