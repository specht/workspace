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

def color(name)
    raise ArgumentError, "color braucht einen Text, zum Beispiel color \"red\", nicht #{name.inspect}" unless name.is_a?(String)

    __turtle('color', name)
end

def width(size) = __turtle('width', __turtle_number('width', size))
