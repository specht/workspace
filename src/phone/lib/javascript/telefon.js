// The Telefonspiel in JavaScript (Node): say, play, keys, music, sleep, hangup, transfer.
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
    const answer = JSON.parse(Buffer.from(bytes).toString('utf8'));
    if (answer.error) throw new Error(answer.error);   // a wrong argument (a time that is none)
    return answer;
}

// the game's name (the phone shows it, the Telefonbuch lists it) and a line about it
function title(text) { command({ title: String(text) }); }
function subtitle(text) { command({ subtitle: String(text) }); }

// speak the text; returns when it has been said. say(text, { interrupt: true })
// lets a key stop it; the key is kept for the next keys(). mood: Thorsten in a
// mood: neutral, amused, angry, disgusted, drunk, sleepy, surprised or whisper.
function say(text, { interrupt = false, mood = null } = {}) {
    command({ say: String(text), ...(interrupt ? { interrupt: true } : {}), ...(mood ? { mood: String(mood) } : {}) });
}

// play a recording of the game's folder (an MP3) in place of the voice;
// returns when it has been heard. play(file, { interrupt: true }) lets a key stop it.
function play(file, { interrupt = false } = {}) {
    command({ play: String(file), ...(interrupt ? { interrupt: true } : {}) });
}

// wait for up to count keys (# ends early); '' after timeout seconds
function keys(count = 1, timeout = 10) {
    return command({ keys: count, timeout: Math.round(timeout * 1000) }).keys || '';
}

// background music (a YouTube link or a file in the game's folder), looped,
// quieter while speaking; music(null) stops it
// music(source, { volume, start, stop }): start and stop in seconds (75) or
// as '1:15', the piece from start to stop, looped; a YouTube link with t=
// starts there. music(source, 0.5) sets only the volume.
function music(source, options = {}) {
    if (typeof options === 'number') options = { volume: options };
    const { volume = 1, start = null, stop = null } = options;
    command({ music: source, volume, start, stop });
}

// wait; the music keeps playing, keys pressed meanwhile are kept
function sleep(seconds) { command({ sleep: Math.round(seconds * 1000) }); }

// end the call
function hangup() { command({ hangup: true }); process.exit(0); }

// forward the call to the published game with this number: the call goes
// on there as if the caller had dialled it, and this game ends
function transfer(number) { command({ transfer: String(number) }); process.exit(0); }

module.exports = { title, subtitle, say, play, keys, music, sleep, hangup, transfer };
