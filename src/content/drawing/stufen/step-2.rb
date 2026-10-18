require 'pixelflow_canvas'

Pixelflow::Canvas.new(320, 180, :palette) do
    set_predefined_palette(:zughy_32)
    set_pixel(160, 90, 10)
end
