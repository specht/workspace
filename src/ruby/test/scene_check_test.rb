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
        scene('balken.ini', "shape = box\nscale = 10, 1, 1\nmove = 0, 25, 0\n")
        assert_equal :correct, check('balken').status, check('balken').message
        scene('balken.ini', "shape = box\nscale = 10, 1, 1\n")
        r = check('balken')
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

    SOLAR = <<~INI
        background = 0
        shade = on
        stroke = off

        fill = gold
        shape = sphere

        group
            move = 300, 0, 0
            rotate = 0, t * 30, 0

            fill = #3a6ee8
            shape = sphere
            scale = 0.3

            group
                move = 60, 0, 0
                rotate = 0, t * 90, 0

                fill = 1, 1, 1
                shape = sphere
                scale = 0.1
            end
        end
    INI

    def test_solar_system_by_colour
        scene('sonnensystem.ini', SOLAR)
        r = check('sonnensystem')
        assert_equal :correct, r.status, r.message
        assert_includes r.read, 'Farbe gold (#ffd700, Gelb)'
        assert_includes r.read, 'Farbe #3a6ee8 (Blau)'
        assert_includes r.read, 'Hintergrund: #000000 (Schwarz)'

        # A white background.
        scene('sonnensystem.ini', SOLAR.sub("background = 0\n", ''))
        r = check('sonnensystem')
        assert_equal :background, r.problem
        assert_includes r.message, 'Der Hintergrund ist #ffffff (Weiß), verlangt ist Schwarz.'

        # A red sun: says what it sees instead.
        scene('sonnensystem.ini', SOLAR.sub('fill = gold', 'fill = red'))
        r = check('sonnensystem')
        assert_equal :missing_object, r.problem
        assert_includes r.message, 'Ich finde keine Kugel in Gelb oder Orange für »Sonne«.'
        assert_includes r.message, 'Kugel in Rot (Zeile 6)'

        # Earth spinning in place instead of orbiting: the hint about the order.
        scene('sonnensystem.ini', SOLAR.sub("move = 300, 0, 0\n    rotate = 0, t * 30, 0", "rotate = 0, t * 30, 0\n    move = 300, 0, 0"))
        r = check('sonnensystem')
        assert_equal :orbit, r.problem
        assert_includes r.message, 'Steht bei dir `rotate` vor `move`?'

        # Two yellow spheres: which one is the sun?
        scene('sonnensystem.ini', SOLAR.sub("fill = #3a6ee8\n", "fill = #3a6ee8\n    shape = sphere\n    fill = yellow\n    move = 0, 200, 0\n\n    fill = #3a6ee8\n"))
        r = check('sonnensystem')
        refute_equal :correct, r.status
    end

    # The tutorial's own solution, which is also the scene of animation3.mp4.
    def test_tutorial_solar_system_solves_the_task
        scene('sonnensystem.ini', File.read(File.expand_path('../../content/anaglyph/sonnensystem.ini', __dir__)))
        r = check('sonnensystem')
        assert_equal :correct, r.status, r.message
    end

    def test_fill_is_a_pen_that_groups_restore
        scene('farben.ini', "fill = red\nshape = box\n\ngroup\n    fill = blue\n    shape = sphere\nend\n\nshape = cone\n")
        dump = SceneCheck.parse(Open3.capture2('sh', '-c', SceneCheck.command('anaglyph/farben.ini', [0]), chdir: @tmp).first)
        assert_equal [{ 'css' => 'red' }, { 'css' => 'blue' }, { 'css' => 'red' }], dump['objects'].map { |o| o['fill'] }
        assert_equal({ 'rgb' => [1, 1, 1] }, dump['background'])
    end

    def test_objects_by_name_still_work
        named = "shape = sphere\nname = sonne\n\ngroup\nmove = 300, 0, 0\nrotate = 0, t * 30, 0\nshape = sphere\nname = erde\nend\n"
        scene('benannt.ini', named)
        task = { 'id' => 'benannt', 'check' => { 'type' => 'scene', 'file' => 'anaglyph/benannt.ini', 'at_times' => [0, 1, 2],
                 'expect' => [{ 'name' => 'sonne', 'moves' => false }, { 'name' => 'erde', 'orbits' => 'sonne' }] } }
        r = TaskCheck.check(task, '', variables: {}, workspace: @ws, runner: @runner)
        assert_equal :correct, r.status, r.message
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

    # Without preview, the server reads the scene in the Workspace.
    def test_gallery_submission_checks_the_scene
        task = Marshal.load(Marshal.dump(@tasks['galerie']))
        task['check'].delete('preview')
        scene('bunt.ini', "shape = box\n")
        r = TaskCheck.check(task, 'anaglyph/bunt.ini', variables: {}, workspace: @ws, runner: @runner)
        assert_equal :invalid, r.status
        assert_includes r.message, 'mindestens 5 verlangt'
        assert_includes r.message, 'steht 1 Objekt;'
        scene('bunt.ini', (["shape = box"] * 5).join("\n\n") + "\n")
        r = TaskCheck.check(task, 'anaglyph/bunt.ini', variables: {}, workspace: @ws, runner: @runner)
        assert_equal :sent, r.status
        refute_match(/prüft der Workspace noch nicht/, r.warnings.join)
    end

    # With preview: anaglyph the card checks the scene in the browser; the
    # hand-in needs no running Workspace.
    def test_previewed_gallery_submission_needs_no_workspace
        assert_equal 'anaglyph', @tasks['galerie']['check']['preview']
        scene('bunt.ini', "shape = box\n")
        never = ->(*) { flunk 'the Workspace was asked' }
        r = TaskCheck.check(@tasks['galerie'], 'anaglyph/bunt.ini', variables: {}, workspace: @ws, runner: never)
        assert_equal :sent, r.status
        refute_match(/prüft der Workspace noch nicht/, r.warnings.join)
    end

    def test_endless_expression_is_stopped
        scene('scene.ini', "shape = sphere\nmove = (() => { while (true) {} })(), 0, 0\n")
        r = check('kugel')
        refute_equal :correct, r.status
    end
end
