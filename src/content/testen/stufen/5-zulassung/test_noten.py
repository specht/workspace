import pytest

from noten import abitur_grade, grade, is_admitted


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


# die Zulassung: Block I, Ergebnisse unter 5 Punkten, 0 Punkte
def test_admitted():
    assert is_admitted([5] * 36)                     # genau 200 Punkte in Block I
    assert not is_admitted([5] * 35 + [4])           # 199 Punkte
    assert is_admitted([4] * 8 + [6] * 32)           # 8 von 40 unter 5 Punkten: genau 20 Prozent
    assert not is_admitted([4] * 9 + [6] * 31)       # 9 von 40
    assert is_admitted([5] * 9 + [6] * 31)           # 5 Punkte liegen nicht unter 5
    assert not is_admitted([0] + [15] * 35)          # ein Ergebnis mit 0 Punkten
