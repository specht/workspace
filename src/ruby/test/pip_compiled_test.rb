require 'minitest/autorun'
require_relative '../include/pip_check'

# What PipCompiled writes and reads back, without a run.
class PipCompiledTest < Minitest::Test
    MAPS = [{ 'map' => "#####\n#P.L#\n#####", 'facing' => 'east' }].freeze
    CHECK = { 'type' => 'pip', 'commands' => %w[move turn_left front_clear cave_map], 'maps' => MAPS, 'goal' => { 'lamp' => true } }.freeze

    # a plan without main gets one, on its first line in Dart and in a line
    # of its own in Java, so line numbers count back
    def test_a_plan_without_main_is_put_into_one
        dart = PipCompiled.program(CHECK.merge('language' => 'dart'), "move();\nmove();\n", MAPS, [0])
        assert_equal "import 'pip.dart'; void main() { move();", dart[:files]['kata.dart'].lines.first.chomp
        assert_includes dart[:files]['pip.dart'], 'bool frontClear()'
        assert_includes dart[:files]['pip.dart'], 'List<String> caveMap()'
        refute_includes dart[:files]['pip.dart'], 'void take()'
        java = PipCompiled.program(CHECK.merge('language' => 'java'), "import java.util.List;\nmove();\n", MAPS, [0])
        source = java[:files]['HsPip.java'].lines
        assert_equal 'void main() {', source[java[:first] - 2].chomp
        assert_equal 'move();', source[java[:first]].chomp
        assert_includes java[:files]['HsPip.java'], "import java.util.List;\n"
        assert_includes java[:files]['Pip.java'], 'public boolean frontClear()'
        # with call, main is neither needed nor run
        call = [{ 'map' => MAPS[0]['map'], 'call' => 'turnRight()', 'expect' => { 'facing' => 'south' } }]
        refute PipCompiled.wrap?("void turnRight() {}\n", call)
        assert_includes PipCompiled.program(CHECK.merge('language' => 'java'), "void turnRight() {}\n", call, [0])[:files]['HsPip.java'], 'turnRight();'
    end

    def test_functions_without_main_are_named
        assert_includes PipCompiled.main_missing('java', "void walk() {\n    move();\n}\nwalk();\n", MAPS), '`void main() { … }`'
        assert_nil PipCompiled.main_missing('java', "move();\n", MAPS)
    end

    def test_the_output_is_read_map_by_map
        out = "\n__PIP_MAP__ 1\nHallo\n\n__PIP_STATUS__ 0\n\n__PIP_END__\n\n__PIP_MAP__ 0\n\n__PIP_STATUS__ 124\nfehler\n__PIP_END__\n"
        sections = PipCompiled.sections(out)
        assert_nil sections[:compile]
        assert_equal 'Hallo', sections[:maps][1][:output].strip
        assert sections[:maps][0][:timed_out]
        assert_equal 'fehler', sections[:maps][0][:errors].strip
        assert_equal "kata.dart:2:3: Error: x\n", PipCompiled.sections("\n__PIP_COMPILE__\nkata.dart:2:3: Error: x\n")[:compile]
        assert_equal 'at main(kata.java:2)', PipCompiled.tidy('java', 'at HsPip.main(HsPip.java:9)', 8, 12)
    end
end
