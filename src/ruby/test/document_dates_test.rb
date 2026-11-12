require 'minitest/autorun'
require 'shellwords'
require 'tmpdir'
require_relative '../include/document_dates'
require_relative '../include/document_check'

# Dates of a starting file filled in on download (DocumentDates), and the
# check that accepts any date where the starting file had one.
class DocumentDatesTest < Minitest::Test
    THURSDAY = Date.new(2026, 11, 12)

    def test_today_and_a_weekday_at_least_a_week_later
        dates = { '2. März' => 'heute', '9. März' => 'Dienstag', '14. März' => 'Donnerstag' }
        assert_equal({ '2. März' => '12. November', '9. März' => '24. November', '14. März' => '19. November' },
                     DocumentDates.resolve(dates, THURSDAY))
        # across the turn of the year
        assert_equal({ '9. März' => '5. Januar' }, DocumentDates.resolve({ '9. März' => 'Dienstag' }, Date.new(2026, 12, 29)))
    end

    def test_spec_errors
        assert_empty DocumentDates.spec_errors({ '2. März' => 'heute', '9. März' => 'Dienstag' })
        refute_empty DocumentDates.spec_errors({ '2. März' => 'morgen' })
        refute_empty DocumentDates.spec_errors({ 'März' => 'heute' })
        refute_empty DocumentDates.spec_errors([])
    end

    def test_any_date_matches_where_the_starting_file_has_one
        assert_match DocumentDates.pattern('14. März'), 'Am Donnerstag, dem 19. November, verkaufen wir'
        assert_match DocumentDates.pattern('großen Pause'), 'in der großen Pause'
        refute_match DocumentDates.pattern('2. März'), 'im Mai fährt die 7b'
        assert_equal 'dem XDATUMX, um', DocumentDates.normalize('dem 24. November, um')
        # not the end of another number
        assert_equal 'am 12. Märzen', DocumentDates.normalize('am 12. Märzen')
    end

    def paragraph(text, align = 'left')
        { 'text' => text, 'plain' => text.strip, 'align' => align, 'portions' => [] }
    end

    def test_the_text_rule_accepts_the_dates_of_the_download
        starter = { 'paragraphs' => [paragraph('2. März'), paragraph('Der Elternabend ist am Dienstag, dem 9. März, um 19 Uhr.')] }
        theirs = [paragraph('12. November', 'right'), paragraph('Der Elternabend ist am Dienstag, dem 24. November, um 19 Uhr.')]
        assert DocumentCheck.rule_text({}, theirs, starter).ok
        shorter = [paragraph('12. November', 'right'), paragraph('Der Elternabend ist am Dienstag um 19 Uhr.')]
        refute DocumentCheck.rule_text({}, shorter, starter).ok
    end

    def test_the_date_is_found_for_its_alignment
        rule = { 'align' => 'right', 'paragraph' => '2. März' }
        item = DocumentCheck.rule_align(rule, [paragraph('Liebe Eltern,'), paragraph('12. November', 'right')], nil)
        assert item.ok
        assert_equal 'Das Datum ist rechtsbündig', item.label
        item = DocumentCheck.rule_align(rule, [paragraph('                12. November')], nil)
        refute item.ok
        assert_includes item.message, '»12. November« steht nur mit Leerzeichen rechts'
    end

    def test_a_starting_file_gets_its_dates
        skip 'no python3' unless system('python3 -c "" 2>/dev/null')
        Dir.mktmpdir do |dir|
            path = File.join(dir, 'x.docx')
            File.binwrite(path, File.binread(File.join(__dir__, '../../static/formatting-marks/01 Einladung.docx')))
            data = DocumentDates.fill(path, { '2. März' => 'heute', '9. März' => 'Dienstag' }, THURSDAY)
            refute_nil data
            out = File.join(dir, 'out.docx')
            File.binwrite(out, data)
            xml = `unzip -p #{out.shellescape} word/document.xml`
            assert_includes xml, '12. November'
            assert_includes xml, 'am Dienstag, dem 24. November, um 19 Uhr'
            refute_includes xml, 'März'
        end
    end
end
