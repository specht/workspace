# The turtle's commands in Python (turtle_worker.js, docs/plan-katas.md,
# »Turtle«). Each one checks what it gets and hands the command to the host,
# _turtle_host(command, value, line): the worker in the browser, a recorder
# in tests. line is the line of the student's program (kata.py). The host
# answers None, or a message why it cannot do the command.
import sys as _sys
import random as _random


def _turtle_line():
    frame = _sys._getframe(1)
    while frame is not None and frame.f_code.co_filename != 'kata.py':
        frame = frame.f_back
    return frame.f_lineno if frame is not None else None


def _turtle(command, value=None):
    error = _turtle_host(command, value, _turtle_line())
    if error:
        raise ValueError(error)


def _turtle_number(command, value):
    if isinstance(value, (int, float)) and not isinstance(value, bool) and value == value and abs(value) != float('inf'):
        return value
    raise TypeError(f'{command} braucht eine Zahl, zum Beispiel {command}(100), nicht {value!r}')


def forward(distance):
    _turtle('forward', _turtle_number('forward', distance))


def back(distance):
    _turtle('back', _turtle_number('back', distance))


def left(angle):
    _turtle('left', _turtle_number('left', angle))


def right(angle):
    _turtle('right', _turtle_number('right', angle))


def pen_up():
    _turtle('pen_up')


def pen_down():
    _turtle('pen_down')


# A colour by name ("red", "#ff8800"), by a number, its hue on the colour
# wheel (0 red, 120 green, 240 blue), or mixed from two: color("gold",
# "red", 0.25) is a quarter of the way from gold to red. The host gets one
# text: the name, "hue:200" or "mix:gold:red:0.25".
def _turtle_color(command, name, other, amount):
    if isinstance(name, (int, float)) and not isinstance(name, bool) and other is None:
        _turtle(command, f'hue:{float(_turtle_number(command, name))}')
    elif isinstance(name, str) and other is None:
        _turtle(command, name)
    elif isinstance(name, str) and isinstance(other, str) and isinstance(amount, (int, float)) and not isinstance(amount, bool):
        _turtle(command, f'mix:{name}:{other}:{float(_turtle_number(command, amount))}')
    else:
        raise TypeError(f'{command} braucht einen Namen wie {command}("red"), eine Zahl wie {command}(200) oder zwei Farben und einen Anteil wie {command}("gold", "red", 0.5)')


def color(name, other=None, amount=None):
    _turtle_color('color', name, other, amount)


# The colour of the paper, with the same colours as color.
def background(name, other=None, amount=None):
    _turtle_color('background', name, other, amount)


def width(size):
    _turtle('width', _turtle_number('width', size))


# Remembers where the turtle is, where it looks, its pen, colour and width;
# pop goes back to the last place remembered, without drawing.
def push():
    _turtle('push')


def pop():
    _turtle('pop')


# A random number from low to high: a whole one for whole numbers
# (random(1, 6) as a die), otherwise one with a fraction.
def random(low, high):
    _turtle_number('random', low)
    _turtle_number('random', high)
    if high < low:
        low, high = high, low
    if isinstance(low, int) and isinstance(high, int):
        return _random.randint(low, high)
    return _random.uniform(low, high)
