# Steganographie: Botschaften in Bildern verstecken. Musterlösung zu
# »Geheime Botschaften in Bildern«, alle Abschnitte in einer Datei.
require 'chunky_png'
require 'set'

# --- Ein Bild ist eine Liste von Zahlen ------------------------------------

# Rot, Grün und Blau jedes Pixels, Zeile für Zeile, in einem Array.
def channels(image)
    image.pixels.flat_map { |c| [ChunkyPNG::Color.r(c), ChunkyPNG::Color.g(c), ChunkyPNG::Color.b(c)] }
end

# Das Bild zu einem solchen Array.
def from_channels(width, height, values)
    ChunkyPNG::Image.new(width, height, values.each_slice(3).map { |r, g, b| ChunkyPNG::Color.rgb(r, g, b) })
end

# Ein Bild aus einem einzigen Bit jedes Kanals: 255, wo es gesetzt ist, sonst 0.
def bit_plane(image, bit = 0)
    from_channels(image.width, image.height, channels(image).map { |v| v[bit] * 255 })
end

# --- Eine Botschaft im untersten Bit ---------------------------------------

# Die Bits einer Botschaft: ihre Länge in Bytes als 32 Bit, dann jedes Byte,
# das höchste Bit zuerst. Die Botschaft ist ein Text oder schon Bytes, etwa
# komprimierte.
def message_bits(message)
    data = message.b.bytes
    ([data.length].pack('N').bytes + data).flat_map { |byte| 7.downto(0).map { |i| byte[i] } }
end

# Versteckt die Botschaft in den untersten depth Bits jedes Kanals.
def hide(image, message, depth = 1)
    bits = message_bits(message)
    values = channels(image)
    if bits.length > values.length * depth
        raise ArgumentError, "Die Botschaft braucht #{bits.length} Bits, ins Bild passen #{values.length * depth}."
    end
    bits.each_slice(depth).with_index do |group, i|
        group += [0] * (depth - group.length)
        number = group.reduce(0) { |n, bit| (n << 1) | bit }
        values[i] = (values[i] >> depth << depth) | number
    end
    from_channels(image.width, image.height, values)
end

# Liest eine Botschaft, die hide versteckt hat; mit raw: true als Bytes.
def reveal(image, depth = 1, raw: false)
    bits = channels(image).flat_map { |v| (depth - 1).downto(0).map { |i| v[i] } }
    number = ->(start, count) { bits[start, count].reduce(0) { |n, bit| (n << 1) | bit } }
    length = number.(0, 32)
    raise ArgumentError, 'In diesem Bild steckt keine Botschaft.' if 32 + 8 * length > bits.length

    data = length.times.map { |i| number.(32 + 8 * i, 8) }.pack('C*')
    raw ? data : data.force_encoding('UTF-8')
end

# --- Entdecken --------------------------------------------------------------

# Der Test von Westfeld und Pfitzmann auf die Zeilen top bis bottom: Wie
# verschieden oft kommen die Werte 2k und 2k+1 vor, verglichen mit dem, was
# der Zufall ergäbe? Um 1 heißt: so gleich, wie es nur zufällige unterste
# Bits machen. Bei einem Foto ohne Botschaft ist die Zahl viel größer.
# Mit width vergleicht er Gruppen von width Werten statt Paaren, die Werte
# k·width bis k·width + width − 1; geteilt durch width − 1 ist eine
# gleichmäßige Gruppe wieder um 1.
def chi_square(image, top = 0, bottom = nil, width = 2)
    bottom ||= image.height
    image = image.crop(0, top, image.width, bottom - top)
    counts = Array.new(256, 0)
    channels(image).each { |v| counts[v] += 1 }
    total = 0.0
    groups = 0
    (256 / width).times do |k|
        group = counts[k * width, width]
        expected = group.sum / width.to_f
        next if expected < 5    # zu seltene Gruppen sagen nichts

        total += group.sum { |c| (c - expected)**2 / expected } / (width - 1)
        groups += 1
    end
    total / groups
end

# --- Farbklassen und eine Zahl mit wechselnder Basis -----------------------

# Jedes Pixel als eine Zahl 0xRRGGBB.
def colors(image)
    image.pixels.map { |c| c >> 8 }
end

def from_colors(width, height, values)
    ChunkyPNG::Image.new(width, height, values.map { |c| (c << 8) | 0xff })
end

# Farbklassen als Würfel im RGB-Würfel: width Werte pro Kanal. Mit width = 2
# ist das genau das unterste Bit; 3, 5 oder 6 gehen genauso.
class BoxClasses
    def initialize(width)
        @width = width
    end

    def cell(color)
        [color >> 16, (color >> 8) & 255, color & 255].map { |v| v / @width }
    end

    # Alle Farben der Klasse, aufsteigend sortiert.
    def members(cell)
        r, g, b = cell.map { |c| (c * @width...[c * @width + @width, 256].min).to_a }
        r.product(g, b).map { |x, y, z| (x << 16) | (y << 8) | z }
    end
end

# Die ersten 11 Pixel sagen im untersten Bit (33 Bit), wie viele Pixel danach
# die Zahl tragen.
HEADER = 11

# Versteckt die Botschaft als eine große Zahl: Jedes Pixel ist eine Stelle,
# seine Klasse bestimmt die Basis, die Farbe in der Klasse die Ziffer.
def embed(image, message, classes)
    number = ("\x01".b + message.b).unpack1('H*').to_i(16)
    values = colors(image)
    used = 0
    (HEADER...values.length).each do |i|
        break if number == 0

        members = classes.members(classes.cell(values[i]))
        number, digit = number.divmod(members.length)
        values[i] = members[digit]
        used += 1
    end
    raise ArgumentError, 'Die Botschaft passt nicht ins Bild.' if number > 0

    bits = (3 * HEADER - 1).downto(0).map { |i| used[i] }
    HEADER.times do |i|
        r, g, b = values[i] >> 16, (values[i] >> 8) & 255, values[i] & 255
        r, g, b = (r & ~1) | bits[3 * i], (g & ~1) | bits[3 * i + 1], (b & ~1) | bits[3 * i + 2]
        values[i] = (r << 16) | (g << 8) | b
    end
    from_colors(image.width, image.height, values)
end

# Liest die Zahl von hinten nach vorn und macht wieder Text daraus.
def extract(image, classes)
    values = colors(image)
    used = 0
    HEADER.times { |i| [16, 8, 0].each { |shift| used = (used << 1) | ((values[i] >> shift) & 1) } }
    number = 0
    (HEADER + used - 1).downto(HEADER) do |i|
        members = classes.members(classes.cell(values[i]))
        number = number * members.length + members.bsearch_index { |m| m >= values[i] }
    end
    hex = number.to_s(16)
    hex = "0#{hex}" if hex.length.odd?
    [hex].pack('H*')[1..].force_encoding('UTF-8')
end

# Wie viele Bits passen hinein? Jedes Pixel trägt log2(Größe seiner Klasse).
def capacity(image, classes)
    colors(image)[HEADER..].sum { |c| Math.log2(classes.members(classes.cell(c)).length) }
end
