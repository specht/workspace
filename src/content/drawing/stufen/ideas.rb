require 'pixelflow_canvas'

Pixelflow::Canvas.new(320, 180, :palette) do
    set_predefined_palette(:zughy_32)
    set_color(21)
    fill_rect(0, 0, 319, 179)
    set_color(15)
    100.times do
        set_pixel(rand(320), rand(120))
    end
    set_color(26)
    fill_circle(260, 40, 20)
    set_color(4)
    fill_rect(40, 110, 100, 170)
    set_color(29)
    fill_triangle(30, 110, 110, 110, 70, 75)
    set_color(16)
    draw_text(130, 150, 'Hallo', '8x13B', 2)
    t = Pixelflow::Turtle.new(self)
    t.x = 180
    t.y = 60
    t.set_color(25)
    5.times do
        t.forward(50)
        t.turn_right(144)
    end
end
