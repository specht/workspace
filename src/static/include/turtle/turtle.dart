// The turtle's commands in Dart (docs/katas.md, »Turtle«). Dart runs on the
// server, in a Kata container (TurtleCompiled): the person's code is
// kata.dart with »import 'turtle.dart';« in front of its first line, and
// each run of a card is an isolate of its own, so that nothing of an earlier
// run is left over. Each command checks what it gets as the browser's
// worker does (turtle_worker.js, host) and writes one line,
// »\u001e command \t value \t line«, which the server reads back
// (TurtleCompiled.parse) and the page replays. What the program prints
// (print) stands between those lines.
import 'dart:async';
import 'dart:io';
import 'dart:math' as math;

const _maxCommands = 200000;
const _maxOutput = 100000;
const _maxWidth = 50;
const _maxDepth = 10000;
// the line of a command: for the first _lines commands each time, then
// every _lineEvery (finding it costs a look at the stack)
const _lines = 5000;
const _lineEvery = 50;
const _mark = '\u001e';

final _out = StringBuffer();
final _flushed = Stopwatch()..start();
RandomAccessFile? _stdout;
Set<String> _colours = {};
var _quiet = false;
var _count = 0;
var _printed = 0;
var _depth = 0;
var _line = 0;
final _random = math.Random();

// ends a run: too many commands, or too much output
class _Stop {}

// what came so far, at most every 200 ms and at the end of a run
void _flush() {
  _stdout ??= File('/dev/stdout').openSync(mode: FileMode.writeOnly);
  _stdout!.writeStringSync(_out.toString());
  _out.clear();
  _flushed.reset();
}

void _record(String text) {
  _out
    ..write(_mark)
    ..write(text)
    ..write('\n');
  if (_flushed.elapsedMilliseconds > 200 || _out.length > 1 << 16) _flush();
}

void _print(String text) {
  if (_quiet) return;
  _out.write('$text\n');
  _printed += text.length + 1;
  if (_printed > _maxOutput) _limit('output');
}

Never _limit(String what) {
  _quiet = true;
  _record('LIMIT\t$what');
  _flush();
  throw _Stop();
}

String _clean(Object? text) => '$text'.replaceAll(RegExp(r'[\t\r\n]'), ' ');

int _lineOf(StackTrace trace) {
  final at = RegExp(r'kata\.dart:(\d+)').firstMatch(trace.toString());
  return at == null ? 0 : int.parse(at.group(1)!);
}

// One run (TurtleCompiled): silent until turtleLoaded when it calls a
// function; false when it ended at a limit, so that no run follows.
bool turtleRun(int run, bool silent, void Function() body) {
  for (final line in File('colors.json').readAsLinesSync()) {
    final at = RegExp(r'"([a-z]+)"\s*:').firstMatch(line);
    if (at != null) _colours.add(at.group(1)!);
  }
  _quiet = silent;
  _record('START\t$run');
  var going = true;
  try {
    runZoned(body, zoneSpecification: ZoneSpecification(print: (self, parent, zone, line) => _print(line)));
  } on _Stop {
    going = false;
  } on StackOverflowError catch (_, trace) {
    _record('ERROR\t${_lineOf(trace)}\tZu tiefe Rekursion: Die Funktion ruft sich zu oft selbst auf.');
  } catch (e, trace) {
    final message = e is ArgumentError ? 'ArgumentError: ${e.message}' : '$e';
    _record('ERROR\t${_lineOf(trace)}\t${_clean(message)}');
  }
  if (going) _record('END\t$run');
  _flush();
  return going;
}

void turtleLoaded() {
  _quiet = false;
}

String _number(num v) => v is double && v == v.roundToDouble() && v.abs() < 1e15 ? '${v.toInt()}' : '$v';

void _send(String command, [String? value]) {
  if (_quiet) return;
  if (command == 'push') {
    if (_depth >= _maxDepth) throw ArgumentError('push: Die Schildkröte merkt sich schon $_maxDepth Stellen. Fehlt irgendwo ein pop?');
    _depth++;
  }
  if (command == 'pop') {
    if (_depth == 0) throw ArgumentError('pop: Die Schildkröte hat sich keine Stelle gemerkt. Vor jedem pop steht ein push.');
    _depth--;
  }
  _count++;
  if (_count > _maxCommands) _limit('commands');
  if (_count <= _lines || _count % _lineEvery == 0) _line = _lineOf(StackTrace.current);
  _record('$command\t${value ?? ''}\t$_line');
}

num _finite(String command, num value) {
  if (value.isFinite) return value;
  throw ArgumentError('$command braucht eine Zahl, zum Beispiel $command(100), nicht $value');
}

// a colour by name or code, as turtle_worker.js knows them (colorHex)
bool _known(String name) {
  final text = name.trim().toLowerCase();
  return _colours.contains(text) || RegExp(r'^#(?:[0-9a-f]{6}|[0-9a-f]{3})$').hasMatch(text);
}

String _unknown(String command, String name) =>
    '$command kennt »$name« nicht: Nimm einen englischen Farbnamen wie \'red\', \'orange\' oder \'darkgreen\' oder eine Farbe wie \'#ff8800\'.';

// colours and widths are checked where they draw, as the worker does: not
// in the program before a call (quiet)
void _colour(String command, Object name, String? other, num? amount) {
  if (_quiet) return;
  if (name is num && other == null) {
    _send(command, 'hue:${_number(_finite(command, name))}');
  } else if (name is String && other == null) {
    if (!_known(name)) throw ArgumentError(_unknown(command, name));
    _send(command, _clean(name));
  } else if (name is String && other != null && amount != null) {
    for (final n in [name, other]) {
      if (!_known(n)) throw ArgumentError(_unknown(command, n));
    }
    _send(command, 'mix:${_clean(name)}:${_clean(other)}:${_number(_finite(command, amount))}');
  } else {
    throw ArgumentError('$command braucht einen Namen wie $command(\'red\'), eine Zahl wie $command(200) '
        'oder zwei Farben und einen Anteil wie $command(\'gold\', \'red\', 0.5)');
  }
}

void forward(num distance) => _send('forward', _number(_finite('forward', distance)));
void back(num distance) => _send('back', _number(_finite('back', distance)));
void left(num angle) => _send('left', _number(_finite('left', angle)));
void right(num angle) => _send('right', _number(_finite('right', angle)));
void penUp() => _send('pen_up');
void penDown() => _send('pen_down');

// A colour by name ('red', '#ff8800'), by a number, its hue on the colour
// wheel (0 red, 120 green, 240 blue), or mixed from two:
// color('gold', 'red', 0.25) is a quarter of the way from gold to red.
void color(Object name, [String? other, num? amount]) => _colour('color', name, other, amount);

// the colour of the paper, with the same colours as color
void background(Object name, [String? other, num? amount]) => _colour('background', name, other, amount);

void width(num size) {
  if (_quiet) return;
  if (!(size > 0 && size <= _maxWidth)) throw ArgumentError('width braucht eine Zahl über 0 und höchstens $_maxWidth, nicht ${_number(size)}.');
  _send('width', _number(size));
}

// Remembers where the turtle is, where it looks, its pen, colour and
// width; pop goes back to the last place remembered, without drawing.
void push() => _send('push');
void pop() => _send('pop');

// A random number from low to high: a whole one for whole numbers
// (random(1, 6) as a die), otherwise one with a fraction.
T random<T extends num>(T low, T high) {
  _finite('random', low);
  _finite('random', high);
  if (high < low) return random(high, low);
  if (low is int && high is int) return (low + _random.nextInt(high - low + 1)) as T;
  return (low + _random.nextDouble() * (high - low)) as T;
}
