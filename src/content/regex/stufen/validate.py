# Eingaben prüfen, bevor ein Programm mit ihnen arbeitet
import re


def is_postcode(text):
    return re.fullmatch(r'\d{5}', text) is not None


for text in ['10115', '1011', 'D-10115 Berlin', '101150']:
    print(repr(text), is_postcode(text))
