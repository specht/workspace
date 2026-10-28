require 'json'

# The turtle's world on the server (docs/plan-katas.md, »Turtle«): the
# same geometry as turtle_canvas.js in the browser. The browser runs the
# student's program (turtle_worker.js) and reports its commands; the world
# replays them into lines. The pictures a task asks for are drawn here too,
# from Ruby in the task file (draw:), with the same commands:
#
#   4.times { forward 100; left 90 }
#
# The turtle starts in the middle (0, 0), looks to the right (0°), x to the
# right, y up; left turns counter-clockwise. The pen starts down, black,
# 2 wide.
class TurtleWorld
    class Error < StandardError; end

    COLORS = JSON.parse(File.read(File.expand_path('../../static/include/turtle/colors.json', __dir__))).freeze
    START_COLOR = '#000000'
    START_WIDTH = 2
    MAX_WIDTH = 50
    MAX_COMMANDS = 200_000
    MAX_DEPTH = 10_000
    COMMANDS = %w[forward back left right pen_up pen_down color width push pop].freeze

    Segment = Struct.new(:x1, :y1, :x2, :y2, :color, :width) do
        def length = Math.hypot(x2 - x1, y2 - y1)
        def to_a = [x1.round(2), y1.round(2), x2.round(2), y2.round(2), color, width]
    end

    attr_reader :segments, :count

    def initialize
        @x = 0.0
        @y = 0.0
        @heading = 0.0
        @pen = true
        @color = START_COLOR
        @width = START_WIDTH
        @segments = []
        @count = 0
        @saved = []
    end

    # #ff8800, #f80, a CSS name, a hue (»hue:200«) or a mix
    # (»mix:gold:red:0.25«), as hex; nil when it is none of them. The same
    # as colorHex in turtle_worker.js.
    def self.color_hex(name)
        text = name.to_s.strip.downcase
        if (hue = text[/\Ahue:(.+)\z/, 1])
            degrees = Float(hue, exception: false)
            return degrees&.finite? ? hue_hex(degrees) : nil
        end
        if (mix = text.match(/\Amix:([^:]+):([^:]+):(.+)\z/))
            a = color_hex(mix[1])
            b = color_hex(mix[2])
            t = Float(mix[3], exception: false)
            return a && b && t&.finite? ? mix_hex(a, b, t) : nil
        end
        return text if text.match?(/\A#\h{6}\z/)
        return "##{text[1] * 2}#{text[2] * 2}#{text[3] * 2}" if text.match?(/\A#\h{3}\z/)

        COLORS[text]
    end

    def self.hex(r, g, b) = '#' + [r, g, b].map { |c| c.clamp(0, 255).round.to_s(16).rjust(2, '0') }.join

    # the hue on the colour wheel: saturation 0.8, lightness 0.47 (HSL)
    def self.hue_hex(degrees)
        h = degrees % 360
        c = (1 - (2 * 0.47 - 1).abs) * 0.8
        x = c * (1 - ((h / 60) % 2 - 1).abs)
        m = 0.47 - c / 2
        r, g, b = if h < 60 then [c, x, 0]
                  elsif h < 120 then [x, c, 0]
                  elsif h < 180 then [0, c, x]
                  elsif h < 240 then [0, x, c]
                  elsif h < 300 then [x, 0, c]
                  else [c, 0, x]
                  end
        hex((r + m) * 255, (g + m) * 255, (b + m) * 255)
    end

    # from a to b, t of the way (0 to 1)
    def self.mix_hex(a, b, t)
        amount = t.clamp(0.0, 1.0)
        part = ->(h, i) { h[1 + 2 * i, 2].to_i(16) }
        hex(*(0..2).map { |i| part.(a, i) + (part.(b, i) - part.(a, i)) * amount })
    end

    # A picture from the task file (trusted Ruby, as the rest of it).
    def self.draw(source)
        world = new
        world.instance_eval(source.to_s, 'draw')
        world
    end

    # The commands the browser reports, [[command, value], …], replayed.
    def self.replay(commands)
        world = new
        raise Error, 'keine Liste von Befehlen' unless commands.is_a?(Array)

        commands.each do |entry|
            raise Error, 'ein Befehl ist keine Liste' unless entry.is_a?(Array) && entry.first.is_a?(String)

            world.command(entry[0], entry[1])
        end
        world
    end

    def command(name, value = nil)
        raise Error, "unbekannter Befehl #{name}" unless COMMANDS.include?(name)

        if %w[forward back left right width].include?(name)
            raise Error, "#{name} ohne Zahl" unless value.is_a?(Numeric) && value.to_f.finite?
        end
        @count += 1
        raise Error, 'zu viele Befehle' if @count > MAX_COMMANDS

        case name
        when 'forward' then move(value.to_f)
        when 'back' then move(-value.to_f)
        when 'left' then @heading += value.to_f
        when 'right' then @heading -= value.to_f
        when 'pen_up' then @pen = false
        when 'pen_down' then @pen = true
        when 'color'
            hex = TurtleWorld.color_hex(value)
            raise Error, "unbekannte Farbe #{value}" unless hex

            @color = hex
        when 'width'
            raise Error, 'Breite außerhalb' unless value > 0 && value <= MAX_WIDTH

            @width = value.to_f
        when 'push'
            raise Error, 'zu viele Stellen gemerkt' if @saved.size >= MAX_DEPTH

            @saved << [@x, @y, @heading, @pen, @color, @width]
        when 'pop'
            raise Error, 'pop ohne push' if @saved.empty?

            @x, @y, @heading, @pen, @color, @width = @saved.pop
        end
        nil
    end

    # The commands as methods, for draw:
    def forward(distance) = command('forward', distance)
    def back(distance) = command('back', distance)
    def left(angle) = command('left', angle)
    def right(angle) = command('right', angle)
    def pen_up = command('pen_up')
    def pen_down = command('pen_down')
    def width(size) = command('width', size)
    def push = command('push')
    def pop = command('pop')

    # color 200 and color 'gold', 'red', 0.5 as in the students' commands
    def color(name, other = nil, amount = nil)
        return command('color', "hue:#{name.to_f}") if name.is_a?(Numeric)
        return command('color', "mix:#{name}:#{other}:#{amount.to_f}") if other

        command('color', name)
    end

    # in draw:, a picture with chance in it is drawn the same each time
    def random(low, high)
        @random ||= Random.new(1)
        low.is_a?(Integer) && high.is_a?(Integer) ? @random.rand(low..high) : low + @random.rand * (high - low)
    end

    private

    def move(distance)
        radians = @heading * Math::PI / 180
        x = @x + distance * Math.cos(radians)
        y = @y + distance * Math.sin(radians)
        @segments << Segment.new(@x, @y, x, y, @color, @width) if @pen && distance != 0
        @x = x
        @y = y
    end
end
