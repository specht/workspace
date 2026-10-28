# The turtle's commands in Ruby (turtle_worker.js, docs/plan-katas.md,
# »Turtle«). Each one checks what it gets and hands the command to the host,
# __turtle_host(command, value, line): the worker in the browser, a
# recorder in tests. line is the line of the student's program (kata.rb).
# The host answers nil, or a message why it cannot do the command.

def __turtle_line
    caller_locations.find { |l| l.path == 'kata.rb' }&.lineno
end

def __turtle(command, value = nil)
    error = __turtle_host(command, value, __turtle_line)
    raise ArgumentError, error if error
    nil
end

def __turtle_number(command, value)
    return value if value.is_a?(Numeric) && value.finite?

    raise ArgumentError, "#{command} braucht eine Zahl, zum Beispiel #{command} 100, nicht #{value.inspect}"
end

def forward(distance) = __turtle('forward', __turtle_number('forward', distance))
def back(distance) = __turtle('back', __turtle_number('back', distance))
def left(angle) = __turtle('left', __turtle_number('left', angle))
def right(angle) = __turtle('right', __turtle_number('right', angle))
def pen_up = __turtle('pen_up')
def pen_down = __turtle('pen_down')

# A colour by name ("red", "#ff8800"), by a number, its hue on the colour
# wheel (0 red, 120 green, 240 blue), or mixed from two: color "gold",
# "red", 0.25 is a quarter of the way from gold to red. The host gets one
# text: the name, "hue:200" or "mix:gold:red:0.25".
def color(name, other = nil, amount = nil)
    if name.is_a?(Numeric) && other.nil?
        __turtle('color', "hue:#{__turtle_number('color', name).to_f}")
    elsif name.is_a?(String) && other.nil?
        __turtle('color', name)
    elsif name.is_a?(String) && other.is_a?(String) && amount.is_a?(Numeric)
        __turtle('color', "mix:#{name}:#{other}:#{__turtle_number('color', amount).to_f}")
    else
        raise ArgumentError, 'color braucht einen Namen wie color "red", eine Zahl wie color 200 oder zwei Farben und einen Anteil wie color "gold", "red", 0.5'
    end
end

def width(size) = __turtle('width', __turtle_number('width', size))

# Remembers where the turtle is, where it looks, its pen, colour and width;
# pop goes back to the last place remembered, without drawing.
def push = __turtle('push')
def pop = __turtle('pop')

# A random number from low to high: a whole one for whole numbers
# (random(1, 6) as a die), otherwise one with a fraction.
def random(low, high)
    __turtle_number('random', low)
    __turtle_number('random', high)
    low, high = high, low if high < low
    low.is_a?(Integer) && high.is_a?(Integer) ? rand(low..high) : low + rand * (high - low)
end
