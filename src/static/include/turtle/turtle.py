# The turtle's commands in Python (turtle_worker.js, docs/plan-katas.md,
# »Turtle«). Each one checks what it gets and hands the command to the host,
# _turtle_host(command, value, line): the worker in the browser, a recorder
# in tests. line is the line of the student's program (kata.py). The host
# answers None, or a message why it cannot do the command.
import sys as _sys


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


def color(name):
    if not isinstance(name, str):
        raise TypeError(f'color braucht einen Text, zum Beispiel color("red"), nicht {name!r}')
    _turtle('color', name)


def width(size):
    _turtle('width', _turtle_number('width', size))
