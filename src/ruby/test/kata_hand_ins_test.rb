require 'minitest/autorun'
require 'tmpdir'
require_relative '../include/kata_hand_ins'
require_relative '../include/submission_store'

class KataHandInsTest < Minitest::Test
    ENTRIES = [
        { :email => 'ada@x', :name => 'Ada', :language => 'ruby', :code_html => '<span class="k">def</span> gcd' },
        { :email => 'ben@x', :name => 'Ben <script>', :language => 'python', :code_html => 'def gcd' },
    ]

    def test_hand_ins_are_numbered_without_names_in_an_order_of_their_own
        html = KataHandIns.html('kata-loops/gcd', 10, 'Der größte gemeinsame Teiler', ENTRIES)
        assert_includes html, '10. Der größte gemeinsame Teiler: 2 Lösungen eingereicht'
        assert_includes html, "data-label='Lösung 1'"
        assert_includes html, "<span class='kata-hand-in-name' hidden>"
        assert_includes html, 'Ben &lt;script&gt;'
        refute_includes html, 'Ben <script>'
        assert_equal html, KataHandIns.html('kata-loops/gcd', 10, 'Der größte gemeinsame Teiler', ENTRIES.reverse)
        assert_equal '', KataHandIns.html('kata-loops/gcd', 10, 'x', [])
    end

    def test_the_code_is_stored_as_a_submission
        Dir.mktmpdir do |dir|
            target = File.join(dir, 'abc.tar.gz')
            code = "def gcd(a, b)\n  b.zero? ? a : gcd(b, a % b) # Euklid\nend\n"
            SubmissionStore.write_text(target, 'kata.rb', code)
            assert_equal ['kata.rb', code], SubmissionStore.read_text(target)
            assert_equal [{ :path => 'kata.rb', :size => code.bytesize }], SubmissionStore.contents(target)
            assert_nil SubmissionStore.read_text(File.join(dir, 'missing.tar.gz'))
        end
    end
end
