#!/usr/bin/env ruby
# palette.rb: macht aus deinen Texturen eine Palette mit 256 Farben, die
# Schattentabelle und die Texturen in dieser Palette, alles in palette.h
# für den Raycaster aus »Raycasting in C«.
#
#   gem install chunky_png     (einmal)
#   ruby palette.rb textures.png
#   ruby palette.rb textures.png --dither    (mit Floyd-Steinberg)
#
# textures.png ist ein Streifen aus sieben quadratischen Kacheln nebeneinander,
# wie ihn das 2D Game Studio speichert: Boden, die fünf Wandarten 1 bis 5,
# Decke. Kacheln mit 8, 16 oder 32 Pixeln werden auf 64 vergrößert.
#
# So entsteht die Palette, und jeder Schritt speichert ein Bild:
#   1. alle Texturen zusammen auf wenige Grundfarben bringen (1-grundfarben.png)
#   2. jede Grundfarbe in 16 Helligkeiten, von voll bis schwarz (2-rampen.png)
#   3. diese Rampen auf 240 Farben bringen; die Farben 0 bis 15 bleiben für
#      die Karte und die Anzeige reserviert (3-palette.png)
#   4. die Texturen in der neuen Palette (4-texturen.png) und abgedunkelt
#      über die Schattentabelle (5-schatten.png)

require 'chunky_png'

TEX = 64
TEXTURES = 7
LEVELS = 16
BASE_COLORS = 40
RESERVED = 16

# die reservierten Farben, in palette.h mit diesen Namen
UI = [
  ['UI_BLACK', [0, 0, 0]],
  ['UI_MAP_FLOOR', [40, 40, 40]],
  ['UI_VIEW', [200, 200, 200]],
  ['UI_PLAYER', [255, 214, 64]],
  ['UI_RAY', [170, 143, 43]],
  ['UI_RAY_SIDE', [64, 152, 184]],
  ['UI_CAMERA_PLANE', [255, 92, 92]],
  ['UI_WHITE', [255, 255, 255]],
]

file = ARGV.find { |a| !a.start_with?('--') } or abort 'Aufruf: ruby palette.rb textures.png [--dither]'
dither = ARGV.include?('--dither')

# --- die Kacheln lesen -----------------------------------------------------

image = ChunkyPNG::Image.from_file(file)
size = image.height
abort "#{file}: #{image.width} x #{size} Pixel; erwartet sind #{TEXTURES} quadratische Kacheln nebeneinander" unless image.width == TEXTURES * size
abort "#{file}: Kacheln mit #{size} Pixeln; erlaubt sind 8, 16, 32 oder 64" unless [8, 16, 32, 64].include?(size)
scale = TEX / size

# tiles[t][y][x] = [r, g, b], auf 64 x 64 vergrößert; Durchsichtiges wird schwarz
tiles = Array.new(TEXTURES) do |t|
  Array.new(TEX) do |y|
    Array.new(TEX) do |x|
      c = image[t * size + x / scale, y / scale]
      a = ChunkyPNG::Color.a(c)
      a < 128 ? [0, 0, 0] : [ChunkyPNG::Color.r(c), ChunkyPNG::Color.g(c), ChunkyPNG::Color.b(c)]
    end
  end
end

# --- Median Cut: viele Farben auf wenige bringen ---------------------------
# Alle Farben liegen in einer Kiste. Die Kiste mit dem größten Unterschied
# wird an ihrer Mitte geteilt, entlang der Farbe (r, g oder b), in der sie am
# weitesten reicht, bis es genug Kisten gibt. Jede Kiste wird zum Durchschnitt
# ihrer Farben. weighted: [[r, g, b], Anzahl]

def median_cut(weighted, count)
  boxes = [weighted]
  while boxes.size < count
    box = boxes.select { |b| b.size > 1 }.max_by { |b| spread(b) * b.sum { |_, w| w } }
    break unless box
    channel = (0..2).max_by { |i| b = box.map { |c, _| c[i] }; b.max - b.min }
    sorted = box.sort_by { |c, _| c[channel] }
    half = sorted.sum { |_, w| w } / 2.0
    total = 0
    cut = sorted.index { |_, w| (total += w) >= half } + 1
    cut = cut.clamp(1, sorted.size - 1)
    boxes.delete_at(boxes.index(box))
    boxes << sorted[0...cut] << sorted[cut..]
  end
  boxes.map do |b|
    weight = b.sum { |_, w| w }
    (0..2).map { |i| (b.sum { |c, w| c[i] * w } / weight.to_f).round }
  end
end

def spread(box)
  (0..2).map { |i| v = box.map { |c, _| c[i] }; v.max - v.min }.max
end

def nearest(palette, rgb, from = 0)
  best = from
  best_difference = Float::INFINITY
  (from...palette.size).each do |i|
    p = palette[i]
    d = (p[0] - rgb[0])**2 + (p[1] - rgb[1])**2 + (p[2] - rgb[2])**2
    if d < best_difference
      best_difference = d
      best = i
    end
  end
  best
end

# ein Bild aus width × height Feldern, jedes cell Pixel breit und cell_height hoch
def save(name, width, height, cell, cell_height = cell)
  png = ChunkyPNG::Image.new(width * cell, height * cell_height, ChunkyPNG::Color::BLACK)
  height.times do |y|
    width.times do |x|
      r, g, b = yield(x, y)
      cell_height.times { |dy| cell.times { |dx| png[x * cell + dx, y * cell_height + dy] = ChunkyPNG::Color.rgb(r, g, b) } }
    end
  end
  png.save(name)
  puts "  #{name}"
end

# --- 1. Grundfarben --------------------------------------------------------

counts = Hash.new(0)
tiles.each { |tile| tile.each { |row| row.each { |c| counts[c] += 1 } } }
base = median_cut(counts.to_a, BASE_COLORS)
puts "#{counts.size} verschiedene Farben in #{file}, #{base.size} Grundfarben"
save('1-grundfarben.png', TEXTURES * TEX, TEX, 2) { |x, y| base[nearest(base, tiles[x / TEX][y][x % TEX])] }

# --- 2. Rampen: jede Grundfarbe in 16 Helligkeiten -------------------------

base_count = Hash.new(0)
counts.each { |c, n| base_count[nearest(base, c)] += n }
ramps = []
base.each_with_index do |c, i|
  LEVELS.times { |level| ramps << [c.map { |v| v * level / (LEVELS - 1) }, base_count[i] + 1] }
end
# sortiert, damit man das Bild lesen kann: erst die Grautöne, dann nach dem
# Farbton, innerhalb davon von dunkel nach hell
def order(c)
  r, g, b = c.map { |v| v / 255.0 }
  max, min = [r, g, b].max, [r, g, b].min
  light = 0.3 * r + 0.59 * g + 0.11 * b
  return [0, 0, light] if max - min < 0.08
  hue = if max == r then ((g - b) / (max - min)) % 6
        elsif max == g then (b - r) / (max - min) + 2
        else (r - g) / (max - min) + 4
        end
  [1, (hue * 2).floor, light]
end

# jede Grundfarbe eine Spalte, oben in voller Helligkeit, unten schwarz,
# nach dem Farbton sortiert; die Felder sind breiter als hoch, so wird das
# Bild ein flaches Band, viermal so breit wie hoch
columns = (0...base.size).sort_by { |i| order(base[i]) }
save('2-rampen.png', base.size, LEVELS, 16, 10) { |x, y| ramps[columns[x] * LEVELS + LEVELS - 1 - y][0] }

# --- 3. die Palette: 16 reservierte Farben und 240 aus den Rampen -----------


reserved = Array.new(RESERVED) { |i| UI[i] ? UI[i][1] : [0, 0, 0] }
palette = reserved + median_cut(ramps, 256 - RESERVED).sort_by { |c| order(c) }
palette << [0, 0, 0] while palette.size < 256
save('3-palette.png', 16, 16, 16) { |x, y| palette[y * 16 + x] }

# --- 4. die Texturen in der Palette ---------------------------------------
# Nur Farben ab 16, die reservierten bleiben der Anzeige. Mit --dither geht
# der Fehler jedes Texels nach Floyd und Steinberg an die Nachbarn rechts
# und darunter; über den Rand hinaus kommt er auf der anderen Seite herein,
# so bleiben die Kacheln nahtlos.

texture = tiles.map do |tile|
  work = tile.map { |row| row.map(&:dup) }
  Array.new(TEX) do |y|
    Array.new(TEX) do |x|
      want = work[y][x].map { |v| v.clamp(0, 255) }
      index = nearest(palette, want, RESERVED)
      if dither
        error = (0..2).map { |i| want[i] - palette[index][i] }
        [[1, 0, 7], [-1, 1, 3], [0, 1, 5], [1, 1, 1]].each do |dx, dy, part|
          ny = y + dy
          next if ny >= TEX
          nx = (x + dx) % TEX
          3.times { |i| work[ny][nx][i] += error[i] * part / 16.0 }
        end
      end
      index
    end
  end
end
save('4-texturen.png', TEXTURES * TEX, TEX, 2) { |x, y| palette[texture[x / TEX][y][x % TEX]] }

# --- die Schattentabelle: dieselbe Farbe mit der Helligkeit light / 15 -----

shade = Array.new(LEVELS) do |light|
  Array.new(256) do |color|
    rgb = palette[color].map { |v| v * light / (LEVELS - 1) }
    color < RESERVED ? nearest(palette, rgb) : nearest(palette, rgb, RESERVED)
  end
end
lights = [15, 11, 7, 4]
save('5-schatten.png', TEXTURES * TEX, lights.size * TEX, 1) do |x, y|
  palette[shade[lights[y / TEX]][texture[x / TEX][y % TEX][x % TEX]]]
end

# --- für die Karte: die mittlere Farbe jeder Textur -------------------------

average = texture.map do |t|
  sum = [0, 0, 0]
  t.each { |row| row.each { |i| 3.times { |k| sum[k] += palette[i][k] } } }
  nearest(palette, sum.map { |v| v / (TEX * TEX) }, RESERVED)
end

# --- palette.h -------------------------------------------------------------

rows = ->(values, per_line) { values.each_slice(per_line).map { |s| '    ' + s.join(', ') + ',' }.join("\n") }
File.open('palette.h', 'w') do |f|
  f.puts "// palette.h: von palette.rb aus #{File.basename(file)} gemacht#{dither ? ' (mit Dithering)' : ''}; nicht von Hand ändern"
  f.puts
  f.puts "#define PALETTE_TEXTURES #{TEXTURES}"
  f.puts "#define PALETTE_TEX #{TEX}"
  f.puts
  f.puts '// die Farben 0 bis 15 sind für die Karte und die Anzeige reserviert'
  f.puts 'enum'
  f.puts '{'
  UI.each_with_index { |(name, _), i| f.puts "    #{name} = #{i}," }
  f.puts '};'
  f.puts
  f.puts '// die 256 Farben: Rot, Grün, Blau'
  f.puts 'static const uint8_t palette_rgb[256][3] = {'
  palette.each_slice(4) { |s| f.puts '    ' + s.map { |c| "{#{c.join(', ')}}" }.join(', ') + ',' }
  f.puts '};'
  f.puts
  f.puts '// die Schattentabelle: palette_shade[light][color] ist color mit der Helligkeit light / 15'
  f.puts "static const uint8_t palette_shade[#{LEVELS}][256] = {"
  shade.each { |row| f.puts '    {'; f.puts rows.(row, 16); f.puts '    },' }
  f.puts '};'
  f.puts
  f.puts '// die Texturen als Farbnummern: Boden, die Wandarten 1 bis 5, Decke'
  f.puts "static const uint8_t palette_texture[PALETTE_TEXTURES][#{TEX}][#{TEX}] = {"
  texture.each do |t|
    f.puts '    {'
    t.each { |row| f.puts '        {' + row.join(', ') + '},' }
    f.puts '    },'
  end
  f.puts '};'
  f.puts
  f.puts '// die mittlere Farbe jeder Textur, für die Karte'
  f.puts "static const uint8_t palette_average[PALETTE_TEXTURES] = {#{average.join(', ')}};"
end
puts '  palette.h'
