require 'minitest/autorun'

# No alert(), confirm() or prompt() of the browser anywhere on the site (the
# maintainer: always proper dialogs): confirm_dialog and input_dialog in
# code.js, or an in-page message. Libraries brought along as they are
# (Monaco, Bootstrap, xterm.js, the compiled TIC-80, …) are left out; comments do not count.
class NoNativeDialogsTest < Minitest::Test
    SRC = File.expand_path('../..', __dir__)
    VENDORED = %r{/(monaco|bootstrap-[\d.]+|bootstrap-icons[^/]*|bower_components|fontawesome|jsqr|katex|nouislider|tabler|wnumb|xterm|runtime|node_modules|tic80)/|\.min\.js\z|\.pkgd\.}
    CALL = /(?<![\w.$])(?:window\.)?(alert|confirm|prompt)\s*\(/

    def files
        Dir[File.join(SRC, 'static', '**', '*.{js,html,mjs}')] +
            Dir[File.join(SRC, 'content', '**', '*.{md,html,js}')] +
            Dir[File.join(SRC, 'ruby', '**', '*.rb')].reject { |f| f.include?('/test/') }
    end

    def test_no_native_dialogs
        found = files.reject { |f| f.match?(VENDORED) }.flat_map do |path|
            File.readlines(path, :encoding => 'UTF-8').each_with_index.filter_map do |line, i|
                text = line.strip
                next if text.start_with?('//', '#', '*', '/*', '<!--')

                "#{path.sub("#{SRC}/", '')}:#{i + 1}: #{text[0, 100]}" if line.match?(CALL)
            end
        end
        assert_empty found, "Use confirm_dialog or input_dialog (code.js) instead of the browser's own:\n" + found.join("\n")
    end
end
