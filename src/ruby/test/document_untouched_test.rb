require 'minitest/autorun'
require_relative '../include/document_check'

# The guard of document tasks: an upload that is still the starting file
# (most likely a second download was edited) is told so. A change only in
# the header, the footer or the pages (a title page, a page turned) is a
# change too.
class DocumentUntouchedTest < Minitest::Test
    def starter
        {
            'paragraphs' => [{ 'text' => 'Projektmappe', 'style' => 'Title', 'portions' => [] }],
            'page_parts' => [{ 'footer' => { 'text' => 'Seite 1', 'portions' => [{ 'type' => 'Text', 'text' => 'Seite ' }, { 'type' => 'TextField', 'field' => 'PageNumber' }] } }],
            'header_boxes' => [{ 'page' => 1 }, { 'page' => 2 }], 'footer_boxes' => [{ 'page' => 1 }, { 'page' => 2 }],
            'pages' => [{ 'width' => 595.3, 'height' => 841.9 }, { 'width' => 595.3, 'height' => 841.9 }],
        }
    end

    def test_the_starting_file_itself_is_untouched
        assert DocumentCheck.untouched?(starter, starter)
        # the starter is kept without the pages' drawings
        assert DocumentCheck.untouched?(starter.merge('pages' => starter['pages'].map { |p| p.merge('svg' => '<svg/>') }), starter)
    end

    def test_a_title_page_or_a_field_or_a_page_turned_is_a_change
        refute DocumentCheck.untouched?(starter.merge('header_boxes' => [{ 'page' => 2 }], 'footer_boxes' => [{ 'page' => 2 }]), starter)
        parts = [{ 'footer' => { 'text' => 'Seite 1 von 2', 'portions' => starter['page_parts'][0]['footer']['portions'] + [{ 'type' => 'TextField', 'field' => 'PageCount' }] } }]
        refute DocumentCheck.untouched?(starter.merge('page_parts' => parts), starter)
        refute DocumentCheck.untouched?(starter.merge('pages' => [starter['pages'][0], { 'width' => 841.9, 'height' => 595.3 }]), starter)
    end
end
