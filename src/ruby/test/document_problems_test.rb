require 'minitest/autorun'
require_relative '../include/document_problems'

# The short line of an open point (DocumentProblems): every rule of a text
# or a presentation has one or says why not, each as short as a line of a
# point met; a short first sentence stays the line; a rule's own wins.
class DocumentProblemsTest < Minitest::Test
    Item = Struct.new(:rule, :label, :ok, :message, :problem, keyword_init: true)

    # rules whose messages begin short already (»In diesem PDF finde ich
    # keine Seite.«), or that are no rule of a check
    WITHOUT = %w[name pdf].freeze

    def rules_in(file)
        File.read(File.expand_path("../include/#{file}", __dir__)).scan(/def self\.rule_(\w+)\(/).flatten
    end

    def test_every_rule_of_text_and_slides_has_a_line
        missing = (rules_in('document_check.rb') + rules_in('document_check_slides.rb')).uniq - WITHOUT - DocumentProblems::LINES.keys
        assert_empty missing, 'a line in DocumentProblems::LINES, or in WITHOUT with the reason'
    end

    def test_each_line_is_short
        labels = { 'align' => '»Sommerfest der 7b« ist zentriert', 'same_as' => 'Wie »Programm« gesetzt',
                   'table_sorted' => 'Nach »Punkte« sortiert, die meisten oben', 'crop' => 'Zu einem Quadrat zugeschnitten',
                   'tabs' => 'Preise an einem Tabstopp', 'wrap' => 'Das Bild steht links neben dem Text',
                   'headings' => 'Überschriften der Ebene 2', 'style_changed' => 'Die Formatvorlage »Überschrift 1« geändert',
                   'columns' => 'Der Text in zwei Spalten', 'header_text' => '»Klassenfahrt« in der Kopfzeile' }
        DocumentProblems::LINES.each do |rule, maker|
            line = maker.call(Item.new(:rule => rule, :label => labels.fetch(rule, 'Etwas'), :message => 'x'))
            assert line.size <= DocumentProblems::SHORT, "#{rule}: #{line}"
            refute_match(/\.\z/, line, rule)
        end
        assert_equal '»Sommerfest der 7b« ist nicht zentriert',
                     DocumentProblems::LINES['align'].call(Item.new(:label => labels['align']))
        assert_equal 'Nicht nach »Punkte« sortiert',
                     DocumentProblems::LINES['table_sorted'].call(Item.new(:label => labels['table_sorted']))
        assert_equal 'Überschriften ohne »Überschrift 2«', DocumentProblems::LINES['headings'].call(Item.new(:label => labels['headings']))
    end

    def test_which_line_the_card_gets
        long = 'An 5 Stellen stehen Leerzeichen hintereinander: »Turnhalle  der Schule«, und weitere. Ein Leerzeichen genügt.'
        assert_equal 'Leerzeichen hintereinander', DocumentProblems.line(Item.new(:rule => 'spaces', :ok => false, :message => long))
        assert_nil DocumentProblems.line(Item.new(:rule => 'align', :ok => false, :message => 'Den Absatz »Ort« finde ich nicht mehr. Lass ihn stehen.'))
        assert_nil DocumentProblems.line(Item.new(:rule => 'spaces', :ok => true, :message => long))
        assert_equal 'D4 ohne Formel', DocumentProblems.line(Item.new(:rule => 'formulas', :ok => false, :message => 'x', :problem => 'D4 ohne Formel'))
        assert_equal 'An 5 Stellen stehen Leerzeichen hintereinander: »Turnhalle  der Schule«, und weitere.', DocumentProblems.first_sentence(long)
    end
end
