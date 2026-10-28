// The turtle's commands in JavaScript (turtle_worker.js, docs/plan-katas.md,
// »Turtle«), as the parameters of the function the student's program runs
// in. Each one checks what it gets and hands the command to the host,
// host(command, value, line): the worker in the browser, a recorder in
// tests. line is the line of the student's program (kata.js), found by the
// host from the stack. The host answers null, or a message why it cannot do
// the command.
function turtleCommands(host) {
    function send(command, value) {
        const error = host(command, value);
        if (error) throw new Error(error);
    }
    function number(command, value) {
        if (typeof value === 'number' && Number.isFinite(value)) return value;
        throw new TypeError(command + ' braucht eine Zahl, zum Beispiel ' + command + '(100), nicht ' + JSON.stringify(value));
    }
    return {
        forward: (distance) => send('forward', number('forward', distance)),
        back: (distance) => send('back', number('back', distance)),
        left: (angle) => send('left', number('left', angle)),
        right: (angle) => send('right', number('right', angle)),
        penUp: () => send('pen_up'),
        penDown: () => send('pen_down'),
        // A colour by name ("red", "#ff8800"), by a number, its hue on the
        // colour wheel (0 red, 120 green, 240 blue), or mixed from two:
        // color("gold", "red", 0.25) is a quarter of the way from gold to
        // red. The host gets one text: the name, "hue:200" or
        // "mix:gold:red:0.25".
        color: (name, other, amount) => {
            if (typeof name === 'number' && other === undefined) send('color', 'hue:' + number('color', name));
            else if (typeof name === 'string' && other === undefined) send('color', name);
            else if (typeof name === 'string' && typeof other === 'string' && typeof amount === 'number') send('color', 'mix:' + name + ':' + other + ':' + number('color', amount));
            else throw new TypeError('color braucht einen Namen wie color("red"), eine Zahl wie color(200) oder zwei Farben und einen Anteil wie color("gold", "red", 0.5)');
        },
        width: (size) => send('width', number('width', size)),
        // Remembers where the turtle is, where it looks, its pen, colour
        // and width; pop goes back to the last place remembered, without
        // drawing.
        push: () => send('push'),
        pop: () => send('pop'),
        // A random number from low to high: a whole one for whole numbers
        // (random(1, 6) as a die), otherwise one with a fraction.
        random: (low, high) => {
            number('random', low);
            number('random', high);
            if (high < low) [low, high] = [high, low];
            if (Number.isInteger(low) && Number.isInteger(high)) return low + Math.floor(Math.random() * (high - low + 1));
            return low + Math.random() * (high - low);
        },
    };
}

if (typeof module !== 'undefined') module.exports = { turtleCommands };
