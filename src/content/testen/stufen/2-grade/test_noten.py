import pytest

from noten import grade


# die Enden: 0 und 15 Punkte
def test_grade_at_the_ends():
    assert grade(0) == "6"
    assert grade(15) == "1+"


def test_grade_in_between():
    assert grade(13) == "1-"
    assert grade(5) == "4"


# gleich neben den Enden: Punkte, die es nicht gibt
def test_grade_outside():
    with pytest.raises(ValueError):
        grade(-1)
    with pytest.raises(ValueError):
        grade(16)
