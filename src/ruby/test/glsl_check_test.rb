require 'minitest/autorun'
require_relative '../include/glsl_check'

# The compile check of GLSL shaders (GlslCheck): the command it runs in the
# Workspace and what it makes of glslangValidator's answer.
class GlslCheckTest < Minitest::Test
    def runner(output, status)
        ->(_command, _seconds) { { :output => output, :status => status, :timed_out => false } }
    end

    def test_the_shader_is_wrapped_as_the_preview_wraps_it
        command = GlslCheck.command('glsl-starter/shader.frag')
        assert_includes command, "'#version 300 es'"
        assert_includes command, "'#line 1'"
        assert_includes command, "cat '/workspace/glsl-starter/shader.frag'"
        assert_includes command, 'mainImage(hackschuleColor, gl_FragCoord.xy)'
        assert_includes command, '| glslangValidator --stdin -S frag'
    end

    def test_only_plain_paths_below_the_workspace
        ["../etc/passwd", "a/../../b", "a'b", "/etc/passwd", "a b"].each do |path|
            assert_raises(ArgumentError, path) { GlslCheck.command(path) }
        end
    end

    def test_a_shader_that_compiles_passes
        assert_nil GlslCheck.check('glsl-starter/shader.frag', runner("stdin\n", 0))
    end

    def test_the_first_error_with_the_students_line
        output = "stdin\nERROR: 0:7: 'inside' : undeclared identifier \nERROR: 0:7: '' : compilation terminated \n" \
                 "ERROR: 2 compilation errors.  No code generated.\n"
        message = GlslCheck.check('glsl-starter/shader.frag', runner(output, 2))
        assert_includes message, 'Zeile 7'
        assert_includes message, "`'inside' : undeclared identifier`"
        assert_includes message, '`shader.frag`'
    end

    def test_no_compiler_or_no_workspace_is_not_held_against_anyone
        assert_nil GlslCheck.check('glsl-starter/shader.frag', runner("sh: 1: glslangValidator: not found\n", 127))
        assert_nil GlslCheck.check('glsl-starter/shader.frag', ->(_c, _s) { nil })
        assert_nil GlslCheck.check('glsl-starter/shader.frag', ->(_c, _s) { { :output => '', :status => 124, :timed_out => true } })
    end
end
