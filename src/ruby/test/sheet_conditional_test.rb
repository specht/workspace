require 'minitest/autorun'
require_relative '../include/document_check'

# Whether a conditional format colours a cell (DocumentCheck.coloured? and
# coloured_text?): conditions on numbers, and on text as LibreOffice and
# Excel store them (»Zellwert ist gleich "Nachzahlung"«, »enthält«, a
# formula G19="Nachzahlung"); what cannot be worked out counts as right.
class SheetConditionalTest < Minitest::Test
    def formats(operator, formula)
        [{ 'ranges' => ['G19:G19'], 'entries' => [{ 'type' => 0, 'operator' => operator, 'formula1' => formula }] }]
    end

    def test_equal_text
        f = formats(0, '"Nachzahlung"')
        assert_equal true, DocumentCheck.coloured_text?(f, 'G19', 'Nachzahlung')
        assert_equal false, DocumentCheck.coloured_text?(f, 'G19', 'Rückzahlung')
        assert_equal true, DocumentCheck.coloured_text?(formats(0, '"nachzahlung"'), 'G19', 'Nachzahlung'), 'as the programs, without case'
    end

    def test_contains_and_formula
        assert_equal true, DocumentCheck.coloured_text?(formats(22, '"Nach"'), 'G19', 'Nachzahlung')
        assert_equal false, DocumentCheck.coloured_text?(formats(22, '"Nach"'), 'G19', 'Rückzahlung')
        assert_equal true, DocumentCheck.coloured_text?(formats(24, '$G$19="Nachzahlung"'), 'G19', 'Nachzahlung')
        assert_nil DocumentCheck.coloured_text?(formats(24, 'G15<0'), 'G19', 'Nachzahlung'), 'another formula counts as right'
    end

    def test_the_wrong_word_and_no_format
        assert_equal false, DocumentCheck.coloured_text?(formats(0, '"Rückzahlung"'), 'G19', 'Nachzahlung')
        assert_equal false, DocumentCheck.coloured_text?(formats(0, '"Nachzahlung"'), 'G18', 'Nachzahlung'), 'another cell'
    end

    def test_numbers
        assert_equal true, DocumentCheck.coloured?(formats(2, '0'), 'G19', 4.0)
        assert_equal false, DocumentCheck.coloured?(formats(2, '9'), 'G19', 8.0)
    end
end
