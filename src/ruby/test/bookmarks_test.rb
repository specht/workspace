require 'minitest/autorun'
require_relative '../include/bookmarks'

# What a bookmark keeps (Bookmarks): the path of a page and the anchor of a
# heading, nothing else, and only in a narrow form.
class BookmarksTest < Minitest::Test
    def test_a_page_is_its_path_without_a_slash_at_the_end
        assert_equal '/video-compression', Bookmarks.page('/video-compression/')
        assert_equal '/audio-compression/3', Bookmarks.page('/audio-compression/3')
        assert_equal '/', Bookmarks.page('/')
    end

    def test_other_pages_are_refused
        assert_nil Bookmarks.page('video-compression')
        assert_nil Bookmarks.page('/seite?x=1')
        assert_nil Bookmarks.page("/#{'a' * 300}")
        assert_nil Bookmarks.page('/<script>')
    end

    def test_an_anchor_is_a_heading_id
        assert_equal 'chroma-subsampling', Bookmarks.anchor('chroma-subsampling')
        assert_equal 'μ-law-und-g711', Bookmarks.anchor('μ-law-und-g711')
        assert_nil Bookmarks.anchor('')
        assert_nil Bookmarks.anchor('a b')
        assert_nil Bookmarks.anchor("x'><img")
    end
end
