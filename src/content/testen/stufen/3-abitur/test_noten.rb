require 'minitest/autorun'
require_relative 'noten'

class NotenTest < Minitest::Test
  # die Enden: 0 und 15 Punkte
  def test_grade_at_the_ends
    assert_equal '6', grade(0)
    assert_equal '1+', grade(15)
  end

  def test_grade_in_between
    assert_equal '1-', grade(13)
    assert_equal '4', grade(5)
  end

  # gleich neben den Enden: Punkte, die es nicht gibt
  def test_grade_outside
    assert_raises(ArgumentError) { grade(-1) }
    assert_raises(ArgumentError) { grade(16) }
  end

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
