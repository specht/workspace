require 'pixelflow_canvas'

Pixelflow::Canvas.new(320, 180, :palette) do
    set_predefined_palette(:zughy_32)
    set_color(16)
    fill_rect(0, 0, 319, 179)
end
