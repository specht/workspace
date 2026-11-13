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

    # the line's list: by title as the book, a hidden game (a part of
    # another one) marked, still in it to be dialled and forwarded to
    def test_the_list_for_the_line_by_title_with_hidden_ones_marked
        games = [{ :id => 'a', :number => '741', :title => 'Der Klammer-Doktor', :file => 'start.rb' },
                 { :id => 'b', :number => '007', :title => '(', :file => '(.rb', :hidden => true },
                 { :id => 'c', :number => '254', :title => 'Ali die Ameise', :file => 'ali.rb' }]
        list = JSON.parse(PhoneGames.manifest(games))['games']
        assert_equal %w[007 254 741], list.map { |g| g['number'] }
        assert_equal [true, nil, nil], list.map { |g| g['hidden'] }
    end

    # numbers are prefix-free, as in a real network: no number is the
    # beginning of another; the reason says which game is in the way and why
    def test_no_number_is_the_beginning_of_another
        games = [{ :id => 'a', :number => '120', :state => 'published', :title => 'Bude' },
                 { :id => 'b', :number => '3423', :word => 'DICE', :state => 'pending', :title => 'Würfel' },
                 { :id => 'c', :number => '777', :state => 'rejected', :title => 'Weg' }]
        assert_nil PhoneGames.prefix_clash('121', games)
        assert_nil PhoneGames.prefix_clash('120', games)               # the number itself: a new version
        assert_nil PhoneGames.prefix_clash('7771', games)              # a rejected game holds nothing
        assert_equal 'a', PhoneGames.prefix_clash('1205', games)[:id]
        assert_equal 'b', PhoneGames.prefix_clash('342', games)[:id]
        assert_nil PhoneGames.prefix_clash('1205', games, :except_id => 'a')   # its own new number
        longer = PhoneGames.prefix_message('1205', nil, games[0])
        assert_includes longer, 'schon bei diesem Spiel, und der Rest wäre seine Durchwahl'
        shorter = PhoneGames.prefix_message('342', nil, games[1])
        assert_includes shorter, 'DICE (3423)'
        assert_includes shorter, 'Wer 3423 wählt, wäre nach 342 schon bei deinem Spiel'
        pairs = PhoneGames.prefix_pairs(games + [{ :id => 'd', :number => '1204', :state => 'published' }])
        assert_equal [%w[a d]], pairs.map { |x, y| [x[:id], y[:id]] }
    end
end
