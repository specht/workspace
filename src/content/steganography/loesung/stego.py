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

def chi_square(image, top=0, bottom=None):
    """Der Test von Westfeld und Pfitzmann auf die Zeilen top bis bottom: Wie
    verschieden oft kommen die Werte 2k und 2k+1 vor, verglichen mit dem, was
    der Zufall ergäbe? Um 1 heißt: so gleich, wie es nur zufällige unterste
    Bits machen. Bei einem Foto ohne Botschaft ist die Zahl viel größer."""
    image = image.crop((0, top, image.width, image.height if bottom is None else bottom))
    counts = [0] * 256
    for v in channels(image):
        counts[v] += 1
    total, pairs = 0.0, 0
    for k in range(128):
        a, b = counts[2 * k], counts[2 * k + 1]
        expected = (a + b) / 2
        if expected >= 5:    # zu seltene Paare sagen nichts
            total += (a - expected) ** 2 / expected + (b - expected) ** 2 / expected
            pairs += 1
    return total / pairs


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


# --- Farben, wie wir sie sehen: CIELab --------------------------------------

def linear(v):
    """Ein Kanal von 0 bis 255 als Lichtmenge von 0 bis 1 (sRGB ohne Gamma)."""
    c = v / 255
    return c / 12.92 if c <= 0.04045 else ((c + 0.055) / 1.055) ** 2.4


LINEAR = [linear(v) for v in range(256)]


def f(t):
    return t ** (1 / 3) if t > 216 / 24389 else (24389 / 27 * t + 16) / 116


def lab(color):
    """Die Farbe 0xRRGGBB in CIELab: L ist die Helligkeit (0 bis 100), a und b die Farbe."""
    r, g, b = LINEAR[color >> 16], LINEAR[(color >> 8) & 255], LINEAR[color & 255]
    x = (0.4124 * r + 0.3576 * g + 0.1805 * b) / 0.95047
    y = 0.2126 * r + 0.7152 * g + 0.0722 * b
    z = (0.0193 * r + 0.1192 * g + 0.9505 * b) / 1.08883
    fx, fy, fz = f(x), f(y), f(z)
    return 116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)


# So weit reichen a und b für die Farben des RGB-Würfels.
A_MIN, A_MAX = -86.2, 98.3
B_MIN, B_MAX = -107.9, 94.5


class LabClasses:
    """Farbklassen als Quader im Lab-Raum: n_l Stufen für die Helligkeit,
    n_a und n_b für die Farbe. prepare sucht die Farben aller Klassen, die in
    einem Bild vorkommen, einmal durch alle 16,7 Millionen Farben."""

    def __init__(self, n_l, n_a, n_b):
        self.n_l, self.n_a, self.n_b = n_l, n_a, n_b
        self.lists = {}

    def cell(self, color):
        l, a, b = lab(color)
        i = min(max(int(l / 100 * self.n_l), 0), self.n_l - 1)
        j = min(max(int((a - A_MIN) / (A_MAX - A_MIN) * self.n_a), 0), self.n_a - 1)
        k = min(max(int((b - B_MIN) / (B_MAX - B_MIN) * self.n_b), 0), self.n_b - 1)
        return (i * self.n_a + j) * self.n_b + k

    def prepare(self, image):
        needed = {self.cell(c) for c in set(colors(image))}
        self.lists = {cell: [] for cell in needed}
        for color in range(1 << 24):
            members = self.lists.get(self.cell(color))
            if members is not None:
                members.append(color)

    def members(self, cell):
        return self.lists[cell]


if __name__ == '__main__':
    image = Image.open('fundstueck.png')
    bit_plane(image, 1).save('ebene-1.png')
    print(reveal(image))
