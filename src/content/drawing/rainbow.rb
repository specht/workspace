require 'pixelflow_canvas.rb'

# Wir erstellen ein neues Pixelflow-Canvas mit einer
# Größe von 320x180 Pixeln und wir möchten eine
# Palette verwenden.
canvas = Pixelflow::Canvas.new(320, 180, :palette) do
    # Wir wählen die Zughy-32-Palette
    set_predefined_palette(:zughy_32)

    # Hintergrund
    set_color(16)
    fill_rect(0, 0, 319, 179)

    # drei grüne Graskurven
    set_color(7)
    draw_cubic_bezier(0, 120, 100, 140, 200, 100, 320, 120)
    set_color(8)
    draw_cubic_bezier(0, 140, 100, 150, 200, 110, 320, 140)
    set_color(9)
    draw_cubic_bezier(0, 170, 110, 140, 220, 140, 320, 170)

    # Grasflächen füllen
    set_color(7)
    flood_fill(0, 130)
    set_color(8)
    flood_fill(0, 150)
    set_color(9)
    flood_fill(0, 179)

    # Maske setzen: nur noch Pixel setzen,
    # die die Farbe 16 (Hintergrund) haben
    set_mask do
        add_color(16)
    end

    # Regenbogen zeichnen
    r = 125
    [28, 27, 26, 8, 19, 23, 16].each do |color|
        set_color(color)
        fill_circle(160, 180, r)
        r -= 5
    end

    # Maske erneuern: nur noch Pixel setzen,
    # die jetzt die Farbe 16 (Hintergrund) haben
    set_mask do
        add_color(16)
    end

    # Himmel zeichnen: Verlauf von dunkelblau
    # nach hellblau
    (0...180).each do |y|
        set_color(20 - 3.5 * (y / 179.0) - rand() * 1.5)
        draw_line(0, y, 319, y)
    end

    # Maske aufheben
    remove_mask()

    # Bild speichern
    save_as_png("rainbow.png")
end
