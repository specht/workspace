# LZW: komprimieren und entpacken, mit einem Wörterbuch, das nie mitgeschickt
# wird. Die Lösung zur Aktivität »Komprimieren mit LZW«.

import sys


def compress(data):
    """Macht aus Bytes eine Liste von Codes."""
    table = {bytes([i]): i for i in range(256)}
    codes = []
    current = b''
    for byte in data:
        longer = current + bytes([byte])
        if longer in table:
            current = longer
        else:
            codes.append(table[current])
            table[longer] = len(table)
            current = bytes([byte])
    if current:
        codes.append(table[current])
    return codes


def decompress(codes):
    """Macht aus einer Liste von Codes wieder die Bytes."""
    table = {i: bytes([i]) for i in range(256)}
    if not codes:
        return b''
    previous = table[codes[0]]
    out = [previous]
    for code in codes[1:]:
        if code in table:
            entry = table[code]
        elif code == len(table):
            # Der Encoder hat den Eintrag gerade erst angelegt
            entry = previous + previous[:1]
        else:
            raise ValueError(f'Unbekannter Code {code}')
        table[len(table)] = previous + entry[:1]
        out.append(entry)
        previous = entry
    return b''.join(out)


def size_in_bytes(codes):
    """Wie viele Bytes die Codes brauchen, wenn jeder so breit ist wie nötig:
    Beim i-ten Code (ab 0) hat die Tabelle 256 + i Einträge, der größte
    mögliche Code ist also 255 + i."""
    bits = sum((255 + i).bit_length() for i in range(len(codes)))
    return (bits + 7) // 8


print(compress(b'ABABABA'))
if len(sys.argv) > 1:
    data = open(sys.argv[1], 'rb').read()
    codes = compress(data)
    assert decompress(codes) == data
    print(f'{len(data)} Bytes, {len(codes)} Codes, {size_in_bytes(codes)} Bytes mit wachsender Codebreite')
