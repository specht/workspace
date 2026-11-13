// The Telefonspiel in JavaScript (Node): say, play, keys, music, sleep, hangup, transfer, call, answer.
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

// forget the keys pressed so far (also digits dialled after this game's
// number that keys() has not read yet)
function clearKeys() { command({ clear_keys: true }); }

// forward the call to the published game with this number (letters as on
// the keys: 'DICE'), extension: digits dialled after it; the call goes on
// there as if the caller had dialled it, and this game ends
function transfer(number, extension = '') { command({ transfer: String(number), extension: String(extension) }); process.exit(0); }

// call the game with this number and wait on hold: the caller hears it
// dialled (with the extension after it) and is with that game until it
// answers with answer(); returns its digits, '' when it ended without
function call(number, extension = '') { return command({ call: String(number), extension: String(extension) }).keys || ''; }

// answer the game that called this one: the digits as key tones, then #,
// and this game ends (dialled by a person, the call ends)
function answer(digits = '') { command({ answer: String(digits) }); process.exit(0); }

module.exports = { title, subtitle, say, play, keys, music, sleep, hangup, transfer, clearKeys, call, answer };
