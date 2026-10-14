require 'minitest/autorun'
require 'json'
require_relative '../include/vscode_settings'

class VscodeSettingsTest < Minitest::Test
    DEFAULTS = JSON.parse(File.read(File.expand_path('../default-vscode-settings.json', __dir__)))
    SUPERSEDED = JSON.parse(File.read(File.expand_path('../superseded-vscode-settings.json', __dir__))).reject { |k, _| k == '//' }

    def test_a_new_workspace_gets_the_defaults
        merged = VscodeSettings.merge({}, DEFAULTS, SUPERSEDED)
        assert_equal '%DIR%/.build', merged['latex-workshop.latex.auxDir']
        assert merged['files.exclude']['**/.build']
    end

    def test_untouched_old_latex_settings_move_to_the_build_folder
        old = { 'latex-workshop.latex.tools' => SUPERSEDED['latex-workshop.latex.tools'].first,
                'latex-workshop.latex.recipes' => SUPERSEDED['latex-workshop.latex.recipes'].first,
                'files.exclude' => { '.ssh' => true, 'mine' => true } }
        merged = VscodeSettings.merge(old, DEFAULTS, SUPERSEDED)
        args = merged['latex-workshop.latex.tools'].first['args']
        assert_includes args, '-auxdir=%AUXDIR%'
        assert_equal ['latexmk (LuaLaTeX)'], merged['latex-workshop.latex.recipes'].map { |r| r['name'] }
        assert merged['files.exclude']['**/*.synctex.gz']
        assert merged['files.exclude']['mine'], 'own exclude entries stay'
    end

    def test_own_choices_stay
        own_tools = [{ 'name' => 'mine', 'command' => 'xelatex', 'args' => ['%DOC%'] }]
        merged = VscodeSettings.merge({ 'latex-workshop.latex.tools' => own_tools,
                                        'files.exclude' => { '**/.build' => false },
                                        'files.autoSave' => 'afterDelay',
                                        'editor.minimap.enabled' => true }, DEFAULTS, SUPERSEDED)
        assert_equal own_tools, merged['latex-workshop.latex.tools']
        assert_equal false, merged['files.exclude']['**/.build']
        assert_equal 'afterDelay', merged['files.autoSave']
        assert_equal true, merged['editor.minimap.enabled']
    end
end
