require 'pixelflow_canvas'

Pixelflow::Canvas.new(64, 1, :palette) do
    (0...64).each do |i|
        set_pixel(i, 0, i)
    end
end
