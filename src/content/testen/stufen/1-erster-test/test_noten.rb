require 'minitest/autorun'
require_relative 'noten'

class NotenTest < Minitest::Test
  def test_grade_at_the_top
    assert_equal '1+', grade(15)
  end
end
