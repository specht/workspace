import pytest

from noten import abitur_grade, grade


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


# an den Grenzen der Tabelle der KMK
def test_abitur_grade_at_the_boundaries():
    assert abitur_grade(900) == 1.0
    assert abitur_grade(823) == 1.0
    assert abitur_grade(822) == 1.1
    assert abitur_grade(805) == 1.1
    assert abitur_grade(804) == 1.2
    assert abitur_grade(300) == 4.0


def test_abitur_grade_outside():
    with pytest.raises(ValueError):
        abitur_grade(299)
    with pytest.raises(ValueError):
        abitur_grade(901)
