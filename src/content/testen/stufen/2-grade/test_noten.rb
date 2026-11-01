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
end
