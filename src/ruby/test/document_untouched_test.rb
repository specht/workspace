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

    # a workbook: the cells, their look, filter and charts
    def workbook
        { 'kind' => 'sheet', 'sheets' => [{ 'cells' => [
            { 'cell' => 'A1', 'type' => 'text', 'text' => 'Posten', 'bold' => true },
            { 'cell' => 'B2', 'type' => 'value', 'value' => 148.0, 'number_format' => { 'format' => 'General' } },
            { 'cell' => 'D2', 'type' => 'empty' },
        ], 'autofilter' => { 'on' => false }, 'charts' => [] }] }
    end

    def test_a_workbook_untouched_and_changed
        assert DocumentCheck.untouched?(workbook, workbook)
        cells = ->(extra) { workbook.merge('sheets' => [workbook['sheets'][0].merge('cells' => workbook['sheets'][0]['cells'] + extra)]) }
        refute DocumentCheck.untouched?(cells.([{ 'cell' => 'D3', 'type' => 'formula', 'formula' => '=B2*C2', 'value' => 0 }]), workbook)
        changed = Marshal.load(Marshal.dump(workbook))
        changed['sheets'][0]['cells'][1]['number_format'] = { 'format' => '#,##0.00 [$€-407]' }
        refute DocumentCheck.untouched?(changed, workbook), 'a number format'
        changed = Marshal.load(Marshal.dump(workbook))
        changed['sheets'][0]['autofilter'] = { 'on' => true }
        refute DocumentCheck.untouched?(changed, workbook), 'a filter'
        refute DocumentCheck.untouched?({ 'paragraphs' => [] }, workbook)
    end
end
