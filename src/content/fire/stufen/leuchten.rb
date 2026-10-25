require 'pixelflow_canvas'

# Der Filterkernel als Tabelle: die Gewichte der Nachbarn von oben links
# bis unten rechts, in der Mitte das Pixel selbst
KERNEL = [
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1],
]
WEIGHT = KERNEL.flatten.sum

Pixelflow::Canvas.new(256, 128, :palette) do
    # Doublebuffering aktivieren
    set_draw_mode(:buffered)

    # Farbverlauf von Schwarz über Violett bis Weiß
    (0...16).each do |i|
        set_palette(i, i * 4, 0, i * 8)
        set_palette(i + 16, i * 4 + 64, i * 2, i * 8 + 128)
        set_palette(i + 32, i * 8 + 128, i * 8 + 32, 255)
        set_palette(i + 48, 255, i * 8 + 128, 255)
    end

    # Winkel der Form, wächst mit jedem Bild
    angle = 0.0

    # Endlosschleife
    loop do
        # Filterkernel auf jedes Pixel anwenden
        (0...128).each do |y|
            (0...256).each do |x|
                # Nachbarn mit den Gewichten aus KERNEL zusammenzählen
                c = 0
                (0..2).each do |row|
                    (0..2).each do |col|
                        c += KERNEL[row][col] * get_pixel(x + col - 1, y + row - 1)
                    end
                end
                # durch die Summe der Gewichte teilen
                c /= WEIGHT

                # Zufällige Variation hinzufügen
                c += rand(7) - 3 if c > 0

                # Wertebereich auf 0 bis 63 begrenzen
                c = c.clamp(0, 63)

                # Pixel setzen
                set_pixel(x, y, c)
            end
        end

        # weißes Quadrat zeichnen, um seine Mitte gedreht
        set_color(63)
        corners = (0...4).map do |k|
            a = angle + k * Math::PI / 2
            [128 + 30 * Math.cos(a), 80 + 30 * Math.sin(a)]
        end
        (0...4).each do |k|
            x0, y0 = corners[k]
            x1, y1 = corners[(k + 1) % 4]
            draw_line(x0, y0, x1, y1)
        end
        angle += 0.03

        # Bild anzeigen
        flip()
    end
end
