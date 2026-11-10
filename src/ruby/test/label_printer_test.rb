require 'minitest/autorun'
require_relative '../include/label_printer'

# The labels of the Tag der offenen Tür (LabelPrinter): what the page may
# send, the ZPL the printer gets, the switch and the wait between labels.
class LabelPrinterTest < Minitest::Test
    def blank = '00' * LabelPrinter::BYTES

    def test_the_picture_has_the_printers_dots
        assert_equal 59, LabelPrinter::ROW_BYTES
        assert_equal 15_812, LabelPrinter::BYTES
        assert LabelPrinter.valid_bits?(blank)
        refute LabelPrinter.valid_bits?(blank[2..])
        refute LabelPrinter.valid_bits?("#{blank[2..]}zz")
        refute LabelPrinter.valid_bits?(nil)
    end

    def test_an_empty_label_is_no_label
        assert LabelPrinter.blank?(blank)
        refute LabelPrinter.blank?("80#{blank[2..]}")
    end

    def test_the_zpl_is_a_graphic_field_as_in_the_old_hackschule
        zpl = LabelPrinter.zpl("ff#{blank[2..]}")
        assert zpl.start_with?('^XA^LT0^LS-189^LH0,0')
        assert_includes zpl, '^FO0,0^GFA,15812,15812,59,FF00'
        assert zpl.end_with?("^FS\n^XZ\n")
    end

    def test_the_switch_is_on_until_its_time
        refute LabelPrinter.on?(nil)
        assert LabelPrinter.on?({ :until => 200 }, 100)
        refute LabelPrinter.on?({ :until => 100 }, 100)
    end

    def test_a_browser_waits_between_two_labels
        last = { 'a' => 100 }
        assert_equal 15, LabelPrinter.browser_wait(last, 'a', 105)
        assert_equal 0, LabelPrinter.browser_wait(last, 'a', 130)
        assert_equal 0, LabelPrinter.browser_wait(last, 'b', 105)
    end
end
