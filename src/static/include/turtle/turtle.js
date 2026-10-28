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
        color: (name) => {
            if (typeof name !== 'string') throw new TypeError('color braucht einen Text, zum Beispiel color("red"), nicht ' + JSON.stringify(name));
            send('color', name);
        },
        width: (size) => send('width', number('width', size)),
    };
}

if (typeof module !== 'undefined') module.exports = { turtleCommands };
