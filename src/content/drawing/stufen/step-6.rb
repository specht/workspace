require 'pixelflow_canvas'

Pixelflow::Canvas.new(320, 180, :palette) do
    set_predefined_palette(:zughy_32)
    set_color(16)
    fill_rect(0, 0, 319, 179)
    set_color(7)
    draw_cubic_bezier(0, 120, 100, 140, 200, 100, 320, 120)
    set_color(8)
    draw_cubic_bezier(0, 140, 100, 150, 200, 110, 320, 140)
    set_color(9)
    draw_cubic_bezier(0, 170, 110, 140, 220, 140, 320, 170)
    set_color(7)
    flood_fill(0, 130)
    set_color(8)
    flood_fill(0, 150)
    set_color(9)
    flood_fill(0, 179)
end
