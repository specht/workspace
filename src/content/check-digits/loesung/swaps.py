from check_digits import check_digit, is_valid_ean

# Zwei verschiedene Ziffern a und b vorn in einem Code, die Prüfziffer dazu,
# dann a und b vertauscht: Merkt is_valid_ean den Zahlendreher?
missed = 0
for a in range(0, 10):
    for b in range(0, 10):
        if a == b:
            continue
        digits = [a, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        code = ''.join(str(d) for d in digits + [check_digit(digits)])
        swapped = code[1] + code[0] + code[2:]
        if is_valid_ean(swapped):
            missed = missed + 1
print(f"Unbemerkt: {missed} von 90")
