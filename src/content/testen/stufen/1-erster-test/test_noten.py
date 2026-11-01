from noten import grade


def test_grade_at_the_top():
    assert grade(15) == "1+"
