# Prüfziffern: Strichcodes (EAN-13) und Bücher (ISBN-10)

def weight(index):
    if index % 2 == 0:
        return 1
    else:
        return 3


def check_digit(digits):
    total = 0
    for i in range(0, 12):
        total = total + weight(i) * digits[i]
    return (10 - total % 10) % 10


def is_valid_ean(code):
    if len(code) != 13:
        return False
    if not all(c in '0123456789' for c in code):
        return False
    digits = [int(c) for c in code]
    return check_digit(digits[0:12]) == digits[12]
