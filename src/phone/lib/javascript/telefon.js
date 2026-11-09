// The Telefonspiel in JavaScript (Node): say, keys, music, sleep, hangup.
//
// Every function writes one command as a line of JSON and waits for the
// answer, so a game is an ordinary program that runs from top to bottom.
// console.log still works: lines that are not JSON are ordinary output.

const fs = require('fs');

function command(data) {
    fs.writeSync(1, JSON.stringify(data) + '\n');
    const bytes = [];
    const one = Buffer.alloc(1);
    for (;;) {
        let n;
        try {
            n = fs.readSync(0, one, 0, 1, null);
        } catch (error) {
            if (error.code === 'EAGAIN') continue;
            throw error;
        }
        if (n === 0) process.exit(0);           // the caller hung up
        if (one[0] === 10) break;
        bytes.push(one[0]);
    }
    return JSON.parse(Buffer.from(bytes).toString('utf8'));
}

// speak the text; returns when it has been said
function say(text) { command({ say: String(text) }); }

// wait for up to count keys (# ends early); '' after timeout seconds
function keys(count = 1, timeout = 10) {
    return command({ keys: count, timeout: Math.round(timeout * 1000) }).keys || '';
}

// background music (a YouTube link or a file in the game's folder), looped,
// quieter while speaking; music(null) stops it
function music(source, volume = 0.5) { command({ music: source, volume }); }

// wait; the music keeps playing, keys pressed meanwhile are kept
function sleep(seconds) { command({ sleep: Math.round(seconds * 1000) }); }

// end the call
function hangup() { command({ hangup: true }); process.exit(0); }

module.exports = { say, keys, music, sleep, hangup };
