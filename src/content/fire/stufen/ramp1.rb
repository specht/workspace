require 'pixelflow_canvas'

Pixelflow::Canvas.new(64, 1, :palette) do
    (0...16).each do |i|
        set_palette(i, i * 8, 0, 0)
    end
    (0...64).each do |i|
        set_pixel(i, 0, i)
    end
end
