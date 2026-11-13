require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/phone_games'

# Publishing a Telefonspiel (PhoneGames): games in /workspace/telefon and,
# for a student with several, in its folders one level down; a game takes
# the files of its own folder along, nothing from elsewhere.
class PhoneGamesTest < Minitest::Test
    def workspace
        Dir.mktmpdir do |root|
            FileUtils.mkdir_p(File.join(root, 'telefon/quiz/tief'))
            File.write(File.join(root, 'telefon/start.rb'), "say 'Hallo'\n")
            File.write(File.join(root, 'telefon/musik.mp3'), 'x')
            File.write(File.join(root, 'telefon/quiz/spiel.py'), "say('Quiz')\n")
            File.write(File.join(root, 'telefon/quiz/fragen.json'), '[]')
            File.write(File.join(root, 'telefon/quiz/tief/zu-tief.rb'), "say 'nein'\n")
            yield WorkspaceFiles.new(root), root
        end
    end

    def test_games_in_the_folder_and_one_level_down
        workspace do |ws, _|
            assert_equal ['start.rb', 'quiz/spiel.py'], PhoneGames.candidates(ws)
        end
    end

    def test_a_game_in_a_folder_takes_that_folder_along
        workspace do |ws, _|
            assert_equal %w[fragen.json spiel.py], PhoneGames.plan(ws, 'quiz/spiel.py').map { |f| f[:name] }.sort
            assert_equal %w[musik.mp3 start.rb], PhoneGames.plan(ws, 'start.rb').map { |f| f[:name] }.sort
            Dir.mktmpdir do |out|
                stub = PhoneGames.const_get(:ROOT)
                PhoneGames.send(:remove_const, :ROOT)
                PhoneGames.const_set(:ROOT, out)
                begin
                    dir = PhoneGames.copy(ws, PhoneGames.plan(ws, 'quiz/spiel.py'), 'abc', 'quiz/spiel.py')
                    assert_equal %w[fragen.json spiel.py], Dir.children(dir).sort
                ensure
                    PhoneGames.send(:remove_const, :ROOT)
                    PhoneGames.const_set(:ROOT, stub)
                end
            end
        end
    end

    def test_nothing_deeper_or_outside
        workspace do |ws, _|
            %w[quiz/tief/zu-tief.rb ../telefon/start.rb quiz/../start.rb /etc/passwd].each do |file|
                assert_raises(PhoneGames::Refused, file) { PhoneGames.plan(ws, file) }
            end
        end
    end

    # a game that crashed: out of the Telefonbuch, its author told what the
    # language said and how to get it back, its number still theirs
    def test_the_mail_about_a_crashed_game
        game = { :title => 'Der Klammer-Doktor', :number => '741', :word => nil }
        subject, html = PhoneGames.mail_broken(game, "start.rb:8: syntax error, unexpected `or' <b>", 'https://example.org/telefonspiel')
        assert_includes subject, 'Der Klammer-Doktor'
        assert_includes html, 'start.rb:8: syntax error'
        assert_includes html, '&lt;b&gt;'
        assert_includes html, 'seine Nummer bleibt deine'
        assert_includes html, 'https://example.org/telefonspiel'
        assert_equal 'aus dem Telefonbuch genommen', PhoneGames::STATE_TEXT['broken']
    end
end
