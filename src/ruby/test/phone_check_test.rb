require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/task_check'
require_relative '../include/task_examples'

# The phone check (PhoneCheck) plays small games with the emulator of the
# repository (telefon --test), as the example tests do.
class PhoneCheckTest < Minitest::Test
    RUBY_GAME = <<~RUBY
        require 'telefon'
        music 'musik.mp3'
        say 'Du stehst vor einer Höhle. Drück 1 für links, 2 für rechts.'
        case keys(1)
        when '1' then say 'Links brennt eine Fackel.'
        when '2' then say 'Rechts rauscht Wasser.'
        else say 'Du bleibst stehen.'
        end
        hangup
    RUBY

    PYTHON_GAME = <<~PYTHON
        from telefon import keys, music, say
        music('musik.mp3')
        say('Du stehst vor einer Höhle. Drück 1 für links, 2 für rechts.')
        k = keys(1)
        if k == '1':
            say('Links brennt eine Fackel.')
        elif k == '2':
            say('Rechts rauscht Wasser.')
        else:
            say('Du bleibst stehen.')
    PYTHON

    CASES = [
        { 'keys' => '', 'says' => ['Höhle'], 'music' => true, 'ends' => false },
        { 'keys' => '1', 'says' => ['Fackel'], 'not_says' => ['Wasser'], 'ends' => true },
        { 'keys' => '2', 'says' => ['Wasser'], 'ends' => true },
    ].freeze

    def task(file, cases = CASES)
        { 'id' => 'hoehle', 'check' => { 'type' => 'phone', 'file' => file, 'cases' => cases } }
    end

    def play(files, file, cases = CASES)
        Dir.mktmpdir do |root|
            files.each do |name, text|
                path = File.join(root, name)
                FileUtils.mkdir_p(File.dirname(path))
                File.write(path, text)
            end
            runner = TaskExamples.local_runner(root)
            TaskCheck.check(task(file, cases), nil, :workspace => WorkspaceFiles.new(root), :guest_runner => runner, :runner => runner)
        end
    end

    def test_a_right_game_passes_in_ruby_and_python
        { 'telefon/spiel.rb' => RUBY_GAME, 'telefon/spiel.py' => PYTHON_GAME }.each do |file, game|
            result = play({ file => game, 'telefon/musik.mp3' => 'x' }, file)
            assert_equal :correct, result.status, "#{file}: #{result.message}\n#{result.read}"
            assert_includes result.read, 'Telefon: Links brennt eine Fackel.'
        end
    end

    def test_a_wrong_answer_names_the_keys_and_the_missing_words
        game = RUBY_GAME.sub("when '1' then say 'Links brennt eine Fackel.'", "when '1' then say 'Rechts rauscht Wasser.'")
        result = play({ 'telefon/spiel.rb' => game, 'telefon/musik.mp3' => 'x' }, 'telefon/spiel.rb')
        assert_equal :wrong, result.status
        assert_equal 'Nach der Taste 1 sagt das Spiel nichts mit »Fackel«.', result.message
    end

    def test_a_crash_shows_the_error
        game = RUBY_GAME.sub("when '2' then say 'Rechts rauscht Wasser.'", "when '2' then say 'Rechts ' + 5")
        result = play({ 'telefon/spiel.rb' => game, 'telefon/musik.mp3' => 'x' }, 'telefon/spiel.rb')
        assert_equal :wrong, result.status
        assert_match(/\ANach der Taste 2 bricht das Spiel in Zeile 6 mit einem Fehler ab: `no implicit conversion.*\(TypeError\)`\z/, result.message)
        python = PYTHON_GAME.sub("say('Rechts rauscht Wasser.')", "say('Rechts ' + 5)")
        result = play({ 'telefon/spiel.py' => python, 'telefon/musik.mp3' => 'x' }, 'telefon/spiel.py')
        assert_equal 'Nach der Taste 2 bricht das Spiel in Zeile 8 mit einem Fehler ab: `TypeError: can only concatenate str (not "int") to str`', result.message
    end

    def test_an_error_inside_a_function_names_its_line
        game = <<~RUBY
            require 'telefon'
            def cave
              say 'Vor der Höhle.'
              say 'Drinnen ' + 5
            end
            cave
        RUBY
        result = play({ 'telefon/spiel.rb' => game }, 'telefon/spiel.rb', [{ 'keys' => '', 'ends' => true }])
        assert_match(/\AOhne eine Taste bricht das Spiel in Zeile 4 mit einem Fehler ab/, result.message)
        python = "from telefon import say\ndef cave():\n    say('Vor der Höhle.')\n    say('Drinnen ' + 5)\ncave()\n"
        result = play({ 'telefon/spiel.py' => python }, 'telefon/spiel.py', [{ 'keys' => '', 'ends' => true }])
        assert_match(/\AOhne eine Taste bricht das Spiel in Zeile 4 mit einem Fehler ab/, result.message)
    end

    def test_a_wrong_time_for_the_music_is_an_error_at_its_line
        game = "require 'telefon'\nmusic 'musik.mp3', start: '1:75'\nsay 'Hallo.'\n"
        result = play({ 'telefon/spiel.rb' => game, 'telefon/musik.mp3' => 'x' }, 'telefon/spiel.rb', [{ 'keys' => '', 'ends' => true }])
        assert_match(/\AOhne eine Taste bricht das Spiel in Zeile 2 mit einem Fehler ab: `music: start »1:75« ist keine Zeit/, result.message)
    end

    def test_a_game_that_goes_on_is_not_over
        game = RUBY_GAME.sub('hangup', "keys(1)\nhangup")
        result = play({ 'telefon/spiel.rb' => game, 'telefon/musik.mp3' => 'x' }, 'telefon/spiel.rb')
        assert_equal :wrong, result.status
        assert_equal 'Nach der Taste 1 fragt das Spiel noch nach einer Taste; hier sollte es zu Ende sein.', result.message
    end

    def test_missing_music_is_named
        result = play({ 'telefon/spiel.rb' => RUBY_GAME }, 'telefon/spiel.rb')
        assert_equal :wrong, result.status
        assert_equal 'Die Musik `musik.mp3` gibt es im Ordner des Spiels nicht.', result.message
    end

    def test_several_keys_and_no_key
        assert_equal 'Ohne eine Taste', PhoneCheck.after_keys('')
        assert_equal 'Nach der Taste 1', PhoneCheck.after_keys('1')
        assert_equal 'Nach den Tasten 1, nichts und dann 3', PhoneCheck.after_keys('1,-,3')
        assert_equal 'Nach den Tasten 123', PhoneCheck.after_keys('123')
    end

    def test_spec_errors
        assert_empty PhoneCheck.spec_errors('cases' => CASES)
        assert_includes PhoneCheck.spec_errors('cases' => []), 'phone needs cases'
        assert_includes PhoneCheck.spec_errors('cases' => [{ 'keys' => '1' }]), 'phone case 1 expects nothing (says, not_says, music or ends)'
        assert_includes PhoneCheck.spec_errors('cases' => [{ 'keys' => 'a', 'ends' => true }]), 'phone case 1: keys may hold 0-9, *, #, - and commas'
        assert(PhoneCheck.spec_errors('cases' => [{ 'says' => ['('] }]).any? { |e| e.include?('no regular expression') })
    end
end
