require 'minitest/autorun'
require_relative 'noten'

class NotenTest < Minitest::Test
  # an den Grenzen der Tabelle der KMK
  def test_abitur_grade_at_the_boundaries
    assert_equal 1.0, abitur_grade(900)
    assert_equal 1.0, abitur_grade(823)
    assert_equal 1.1, abitur_grade(822)
    assert_equal 1.1, abitur_grade(805)
    assert_equal 1.2, abitur_grade(804)
    assert_equal 4.0, abitur_grade(300)
  end

  def test_abitur_grade_outside
    assert_raises(ArgumentError) { abitur_grade(299) }
    assert_raises(ArgumentError) { abitur_grade(901) }
  end
end
