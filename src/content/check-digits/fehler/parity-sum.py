# Häufiger Fehler: die Zahl der Einsen statt ihres Rests bei der Division durch 2

def parity(bits):
    ones = 0
    for bit in bits:
        ones = ones + bit
    return ones
