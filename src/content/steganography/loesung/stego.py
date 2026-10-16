# Steganographie: Botschaften in Bildern verstecken. Musterlösung zu
# »Geheime Botschaften in Bildern«, alle Abschnitte in einer Datei.
import math
from PIL import Image


# --- Ein Bild ist eine Liste von Zahlen ------------------------------------

def channels(image):
    """Rot, Grün und Blau jedes Pixels, Zeile für Zeile, in einer Liste."""
    return list(image.convert('RGB').tobytes())


def from_channels(width, height, values):
    """Das Bild zu einer solchen Liste."""
    return Image.frombytes('RGB', (width, height), bytes(values))


def bit_plane(image, bit=0):
    """Ein Bild aus einem einzigen Bit jedes Kanals: 255, wo es gesetzt ist, sonst 0."""
    values = [255 if (v >> bit) & 1 else 0 for v in channels(image)]
    return from_channels(image.width, image.height, values)


# --- Eine Botschaft im untersten Bit ---------------------------------------

def message_bits(message):
    """Die Bits einer Botschaft: ihre Länge in Bytes als 32 Bit, dann jedes Byte, das höchste Bit zuerst.
    Die Botschaft ist ein Text oder schon Bytes, etwa komprimierte."""
    data = message.encode() if isinstance(message, str) else bytes(message)
    data = len(data).to_bytes(4, 'big') + data
    return [(byte >> i) & 1 for byte in data for i in range(7, -1, -1)]


def hide(image, message, depth=1):
    """Versteckt die Botschaft in den untersten depth Bits jedes Kanals."""
    bits = message_bits(message)
    values = channels(image)
    if len(bits) > len(values) * depth:
        raise ValueError(f'Die Botschaft braucht {len(bits)} Bits, ins Bild passen {len(values) * depth}.')
    for k in range(0, len(bits), depth):
        group = bits[k:k + depth]
        group += [0] * (depth - len(group))
        number = 0
        for bit in group:
            number = (number << 1) | bit
        i = k // depth
        values[i] = (values[i] >> depth << depth) | number
    return from_channels(image.width, image.height, values)


def reveal(image, depth=1, raw=False):
    """Liest eine Botschaft, die hide versteckt hat; mit raw=True als Bytes."""
    bits = [(v >> i) & 1 for v in channels(image) for i in range(depth - 1, -1, -1)]

    def number(start, count):
        n = 0
        for bit in bits[start:start + count]:
            n = (n << 1) | bit
        return n

    length = number(0, 32)
    if 32 + 8 * length > len(bits):
        raise ValueError('In diesem Bild steckt keine Botschaft.')
    data = bytes(number(32 + 8 * i, 8) for i in range(length))
    return data if raw else data.decode()


# --- Entdecken --------------------------------------------------------------

def chi_square(image, top=0, bottom=None, width=2):
    """Der Test von Westfeld und Pfitzmann auf die Zeilen top bis bottom: Wie
    verschieden oft kommen die Werte 2k und 2k+1 vor, verglichen mit dem, was
    der Zufall ergäbe? Um 1 heißt: so gleich, wie es nur zufällige unterste
    Bits machen. Bei einem Foto ohne Botschaft ist die Zahl viel größer.
    Mit width vergleicht er Gruppen von width Werten statt Paaren, die Werte
    k·width bis k·width + width − 1; geteilt durch width − 1 ist eine
    gleichmäßige Gruppe wieder um 1."""
    image = image.crop((0, top, image.width, image.height if bottom is None else bottom))
    counts = [0] * 256
    for v in channels(image):
        counts[v] += 1
    total, groups = 0.0, 0
    for k in range(256 // width):
        group = counts[k * width:(k + 1) * width]
        expected = sum(group) / width
        if expected >= 5:    # zu seltene Gruppen sagen nichts
            total += sum((c - expected) ** 2 / expected for c in group) / (width - 1)
            groups += 1
    return total / groups


# --- Farbklassen und eine Zahl mit wechselnder Basis -----------------------

def colors(image):
    """Jedes Pixel als eine Zahl 0xRRGGBB."""
    data = image.convert('RGB').tobytes()
    return [(data[i] << 16) | (data[i + 1] << 8) | data[i + 2] for i in range(0, len(data), 3)]


def from_colors(width, height, values):
    return Image.frombytes('RGB', (width, height), bytes(v for c in values for v in (c >> 16, (c >> 8) & 255, c & 255)))


class BoxClasses:
    """Farbklassen als Würfel im RGB-Würfel: width Werte pro Kanal. Mit width = 2
    ist das genau das unterste Bit; 3, 5 oder 6 gehen genauso."""

    def __init__(self, width):
        self.width = width

    def cell(self, color):
        w = self.width
        return ((color >> 16) // w, ((color >> 8) & 255) // w, (color & 255) // w)

    def members(self, cell):
        """Alle Farben der Klasse, aufsteigend sortiert."""
        w = self.width
        r, g, b = [range(c * w, min(c * w + w, 256)) for c in cell]
        return [(x << 16) | (y << 8) | z for x in r for y in g for z in b]


# Die ersten 11 Pixel sagen im untersten Bit (33 Bit), wie viele Pixel danach
# die Zahl tragen.
HEADER = 11


def embed(image, message, classes):
    """Versteckt die Botschaft als eine große Zahl: Jedes Pixel ist eine Stelle,
    seine Klasse bestimmt die Basis, die Farbe in der Klasse die Ziffer."""
    data = message.encode() if isinstance(message, str) else bytes(message)
    number = int.from_bytes(b'\x01' + data, 'big')
    values = colors(image)
    used = 0
    for i in range(HEADER, len(values)):
        if number == 0:
            break
        members = classes.members(classes.cell(values[i]))
        number, digit = divmod(number, len(members))
        values[i] = members[digit]
        used += 1
    if number > 0:
        raise ValueError('Die Botschaft passt nicht ins Bild.')
    bits = [(used >> i) & 1 for i in range(3 * HEADER - 1, -1, -1)]
    for i in range(HEADER):
        r, g, b = values[i] >> 16, (values[i] >> 8) & 255, values[i] & 255
        r, g, b = (r & ~1) | bits[3 * i], (g & ~1) | bits[3 * i + 1], (b & ~1) | bits[3 * i + 2]
        values[i] = (r << 16) | (g << 8) | b
    return from_colors(image.width, image.height, values)


def extract(image, classes):
    """Liest die Zahl von hinten nach vorn und macht wieder Text daraus."""
    values = colors(image)
    used = 0
    for i in range(HEADER):
        for shift in (16, 8, 0):
            used = (used << 1) | ((values[i] >> shift) & 1)
    number = 0
    for i in range(HEADER + used - 1, HEADER - 1, -1):
        members = classes.members(classes.cell(values[i]))
        number = number * len(members) + members.index(values[i])
    data = number.to_bytes((number.bit_length() + 7) // 8, 'big')
    return data[1:].decode()


def capacity(image, classes):
    """Wie viele Bits passen hinein? Jedes Pixel trägt log2(Größe seiner Klasse)."""
    return sum(math.log2(len(classes.members(classes.cell(c)))) for c in colors(image)[HEADER:])
