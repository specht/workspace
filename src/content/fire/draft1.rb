require 'pixelflow_canvas'

Pixelflow::Canvas.new(256, 128, :palette) do
    # Doublebuffering aktivieren
    set_draw_mode(:buffered)

    # Farbverlauf erstellen
    (0...16).each do |i|
        set_palette(i, i * 8, 0, 0)
        set_palette(i + 16, i * 8 + 128, i * 8, 0)
        set_palette(i + 32, 255, i * 8 + 128, i * 8)
        set_palette(i + 48, 255, 255, i * 8 + 128)
    end

    # Endlosschleife
    loop do
        # heißes Rechteck am unteren Bildschirmrand zeichnen
        set_color(63)
        fill_rect(10, 126, 245, 127)

        # Bild anzeigen
        flip()
    end
end