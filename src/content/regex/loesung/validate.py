# Eingaben prüfen, bevor ein Programm mit ihnen arbeitet
import re


def is_postcode(text):
    return re.fullmatch(r'\d{5}', text) is not None


# Ein Datum wie 24.12.2026: Tag 01 bis 31, Monat 01 bis 12, ein Jahr mit vier Ziffern
def is_date(text):
    return re.fullmatch(r'(0[1-9]|[12][0-9]|3[01])\.(0[1-9]|1[0-2])\.\d{4}', text) is not None


# 3 bis 16 Zeichen: kleine Buchstaben, Ziffern und _, vorn ein Buchstabe
def is_username(text):
    return re.fullmatch(r'[a-z][a-z0-9_]{2,15}', text) is not None


for text in ['10115', '1011', 'D-10115 Berlin', '101150']:
    print(repr(text), is_postcode(text))
for text in ['24.12.2026', '32.01.2026', '1.1.2026', '31.02.2026']:
    print(repr(text), is_date(text))
