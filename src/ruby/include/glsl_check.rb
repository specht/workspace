# A fragment shader of »Shader programmieren mit GLSL« compiled in the
# Workspace, after a files check found what it looks for (expect: compile:
# glsl, TaskCheck.check_workspace). The shader is wrapped as the preview
# wraps it (glsl/vorlage/index.html, WebGL 2): the version, the precision,
# the uniforms in front, a main that calls mainImage behind. #line 1 makes
# the compiler count the student's own lines. glslangValidator reads it all
# from a pipe; nothing is written. Plain Ruby, tested on its own
# (test/glsl_check_test.rb).
module GlslCheck
    HEADER = [
        '#version 300 es',
        'precision highp float;',
        'out vec4 hackschuleColor;',
        'uniform vec3 iResolution;',
        'uniform float iTime;',
        'uniform vec4 iMouse;',
        '#line 1',
    ].freeze
    FOOTER = 'void main() { mainImage(hackschuleColor, gl_FragCoord.xy); }'
    PATH = %r{\A[\w.-]+(?:/[\w.-]+)*\z}

    # The shell command for the file at path below /workspace.
    def self.command(path)
        raise ArgumentError, "not a path: #{path}" unless path.to_s =~ PATH && !path.split('/').include?('..')

        lines = HEADER.map { |l| "'#{l}'" }.join(' ')
        "{ printf '%s\\n' #{lines}; cat '/workspace/#{path}'; printf '\\n%s\\n' '#{FOOTER}'; } | glslangValidator --stdin -S frag"
    end

    # nil when the shader compiles or there is no compiler to ask (a
    # Workspace from before glslang-tools); else [line, message] of the
    # first error, the line counted in the student's file (nil if none).
    def self.error(output, status)
        return nil if status.to_i.zero?
        return nil if status.to_i == 127 || output.to_s.include?('glslangValidator: not found') || output.to_s.include?('command not found')

        first = output.to_s.lines.find { |l| l.start_with?('ERROR:') && !l.include?('compilation errors') }
        return [nil, output.to_s.strip.lines.first.to_s.strip] unless first

        m = first.strip.match(/\AERROR:\s*(?:[^:\s]*:)?(\d+):\s*(.+)\z/)
        m ? [m[1].to_i, m[2].strip] : [nil, first.sub('ERROR:', '').strip]
    end

    # A message for the student, or nil when all is well.
    def self.check(path, runner)
        ran = runner.call(command(path), 20)
        return nil if ran.nil? || ran[:timed_out]

        line, message = error(ran[:output], ran[:status])
        return nil unless message

        where = line ? "Zeile #{line}" : 'eine Stelle'
        "`#{File.basename(path)}` lässt sich so nicht übersetzen, der Compiler meldet #{where}: `#{message}`."
    end
end
