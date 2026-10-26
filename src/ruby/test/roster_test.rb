require 'minitest/autorun'
require 'set'
require_relative '../include/roster'
require_relative 'support/legacy_invitations'

class RosterTest < Minitest::Test
    def roster(texts, admins)
        parsed = Roster.parse_files(texts)
        Roster.build(:groups => parsed[:groups], :members => parsed[:members], :designated => parsed[:designated], :admins => admins)
    end

    def assert_same_as_legacy(texts, admins = ['admin@example.org'])
        expected = LegacyInvitations.load(texts, admins)
        actual = roster(texts, admins)
        expected.each_key do |key|
            assert_equal expected[key], actual[key], "#{key} differs"
            # Order matters where Main iterates (groups, members, names).
            assert_equal expected[key].to_a, actual[key].to_a, "#{key} differs in order" if expected[key].respond_to?(:to_a)
        end
    end

    def test_the_documented_example
        assert_same_as_legacy([<<~END_OF_TEXT])
            > Klasse 12
            Lena Muster <lena@example.org>
            Tom Beispiel <tom@example.org>
            + lehrer@example.org

            > LK Informatik
            lena@example.org
            Mia Test <mia@example.org>
            + lehrer@example.org

            > Lehrkräfte
            Dr. Erika Beispiel <beispiel@example.org>
            Lea Lehrerin <lea@example.org>
            * lea@example.org
        END_OF_TEXT
    end

    def test_quirks_of_the_old_loader
        texts = [
            "lost@example.org\n+ early@example.org\n# a comment\n\n> A\nOne <one@example.org>\n> B\ntwo@example.org\nOne Again <ONE@example.org>\n",
            "continued@example.org\n> A\nTwo Named <two@example.org>\n+ <teacher@example.org>\n+ not an address\n* designated@example.org\n",
            "> Empty\n> B\n  indented@example.org\nDouble  Space <spaces@example.org>\nadmin@example.org\n",
        ]
        assert_same_as_legacy(texts)
        assert_same_as_legacy(texts, [])
        assert_same_as_legacy(texts.reverse)
    end

    def roundtrip(texts)
        parsed = Roster.parse_files(texts)
        records = Roster.records_from_files(parsed, :school => 'home')
        from = Roster.from_records(records)
        before = Roster.content(roster(texts, []))
        after = Roster.content(Roster.build(:groups => from[:groups], :members => from[:members], :designated => from[:designated], :admins => []))
        [before, after]
    end

    def test_the_import_keeps_the_content
        texts = [
            "lost@example.org\n+ early@example.org\n> A\nOne <one@example.org>\n> B\ntwo@example.org\nOne Again <ONE@example.org>\n",
            "> A\nTwo Named <two@example.org>\n+ teacher@example.org\n* designated@example.org\n* one@example.org\n> C\n+ only.plus@example.org\n",
            "> Lehrkräfte\nTeacher <teacher@example.org>\n",
        ]
        before, after = roundtrip(texts)
        assert_equal [], Roster.differences(before, after)
        assert_equal before, after
        refute_includes after[:names].keys, 'only.plus@example.org'
        refute_includes after[:names].keys, 'designated@example.org'
    end

    def test_differences_say_what_changed
        before, = roundtrip(["> A\nOne <one@example.org>\n+ t@example.org\nt@example.org\n"])
        _, after = roundtrip(["> A\nUno <one@example.org>\n> B\nt@example.org\n"])
        lines = Roster.differences(before, after)
        assert_includes lines, 'one@example.org: "One" → "Uno"'
        assert_includes lines, 'Gruppe A: fehlt t@example.org'
        assert(lines.any? { |l| l.start_with?('t@example.org betreut') })
    end

    def test_students_of_a_school_not_yet_approved_can_still_log_in
        records = {
            :groups => [{ :name => '10a', :school => 'home', :approved => true }, { :name => 'BS 1', :school => 'bs', :approved => false }],
            :invitations => [
                { :email => 'a@example.org', :name => 'A', :teacher => false },
                { :email => 'b@example.org', :name => nil, :teacher => false },
                { :email => 'ext@example.org', :name => 'Extern', :teacher => true, :school => 'bs' },
            ],
            :memberships => [['a@example.org', '10a'], ['b@example.org', 'BS 1'], ['a@example.org', 'BS 1']],
            :teaching => [['ext@example.org', 'BS 1']],
        }
        from = Roster.from_records(records)
        built = Roster.build(:groups => from[:groups], :members => from[:members], :designated => from[:designated], :admins => [])
        assert_equal %w[a@example.org b@example.org ext@example.org], built[:invitations].keys.sort
        assert_equal ['ext@example.org'], built[:group_members]['Lehrkräfte']
        assert_includes built[:teacher_emails], 'ext@example.org'
        assert_equal Set['BS 1'], built[:teachers]['ext@example.org']
        assert_equal %w[a@example.org b@example.org], built[:group_members]['BS 1'].sort
    end

    def test_pasted_addresses
        parsed = Roster.parse_member_lines(<<~END_OF_TEXT)
            Lena Muster <Lena@Example.org>
            tom@example.org
            mia@example.org Mia Neu
            "Kai", kai@example.org
            Kai Kollege <kai@example.org>
            Ole	oele@example.org

            keine Adresse hier
            zwei@at@example.org
        END_OF_TEXT
        assert_equal [
            ['lena@example.org', 'Lena Muster'], ['tom@example.org', nil], ['mia@example.org', 'Mia Neu'],
            ['kai@example.org', 'Kai'], ['oele@example.org', 'Ole'],
        ], parsed[:people]
        assert_equal ['keine Adresse hier', 'zwei@at@example.org'], parsed[:rejected]
    end

    # The files in data/invitations of this checkout (development: the
    # e2e and screenshot fixtures; on a copy of production: the real ones).
    def test_the_invitation_files_here
        dir = ENV['INVITATIONS_DIR'] || File.expand_path('../../../data/invitations', __dir__)
        paths = Dir[File.join(dir, '*.txt')].sort.reject { |p| File.basename(p) == '_template.txt' }
        skip "no invitation files in #{dir}" if paths.empty?
        texts = paths.map { |p| File.read(p) }
        assert_same_as_legacy(texts)
        before, after = roundtrip(texts)
        assert_equal [], Roster.differences(before, after)
    end
end
