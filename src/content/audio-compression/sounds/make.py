"""The sounds of »Audio komprimieren«, made here and not taken from
anywhere, so they are free to use. Students fetch them into their Workspace
from src/static/audio/, and the widgets of the page play the same files:

- stimme.wav: Thorsten (Piper, CC0) says a German pangram, with s, f, sch
  and z, the sounds a low sample rate loses first.
- musik.wav: eight seconds of a band made of numbers: drums (the hi-hat
  reaches up to 16 kHz), bass, chords and a melody, A minor, 120 beats per
  minute.
- waehlen.wav: a telephone number dialled with the tones of the keys
  (DTMF), at 8.000 Hz like a telephone line, with a little noise and pauses
  of different length. The number is one of those the Bundesnetzagentur
  keeps free for films and books (»Drama Numbers«, Berlin 030 23125-xxx),
  so nobody answers it; the page does not give it away.
- kastagnetten.wav: two seconds of castanets, clicks with a short ring
  near 2.800 Hz, over a quiet chord; between some clicks a pause of a
  third of a second or more, where a pre-echo is heard alone.
- musik/*.flac: the six instruments of musik.wav one by one (kick, snare,
  hi-hat, bass, chords, melody), each scaled and in the room as in
  the mix, so that they add up to it; for the switches of the spectrogram
  widget. Made by the name »spuren«.

All are mono, 16 bit, a plain 44-byte header (as the students' write_wav
writes it), the first two at 44.100 Hz. Runs in the image of the speech
service, which has numpy, soxr and Thorsten; names choose which files to
write (Piper does not speak the same way twice, so stimme.wav changes
whenever it is made again):

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo \\
        --entrypoint python3 workspace_phone_speech src/content/audio-compression/sounds/make.py [stimme musik spuren waehlen kastagnetten]
"""

import os
import struct
import subprocess
import sys

import numpy as np
import soxr

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, '..', '..', '..', 'static', 'audio')
RATE = 44100
rng = np.random.default_rng(11)


def write_wav(path, samples, rate=RATE):
    data = (np.clip(samples, -1, 1) * 32767).round().astype('<i2').tobytes()
    header = (struct.pack('<4sI4s', b'RIFF', 36 + len(data), b'WAVE')
              + struct.pack('<4sIHHIIHH', b'fmt ', 16, 1, 1, rate, 2 * rate, 2, 16)
              + struct.pack('<4sI', b'data', len(data)))
    with open(path, 'wb') as f:
        f.write(header + data)
    print(path, len(header + data), 'bytes', f'{len(samples) / rate:.2f} s')


def band(signal, low, high):
    """only the frequencies between low and high"""
    spectrum = np.fft.rfft(signal)
    freqs = np.fft.rfftfreq(len(signal), 1 / RATE)
    spectrum[(freqs < low) | (freqs > high)] = 0
    return np.fft.irfft(spectrum, len(signal))


def seconds(s):
    return np.arange(int(s * RATE)) / RATE


def note(name):
    """'A4' and the like: the frequency in equal temperament, A4 = 440 Hz"""
    steps = {'C': -9, 'D': -7, 'E': -5, 'F': -4, 'G': -2, 'A': 0, 'B': 2}
    semitone = steps[name[0]] + (1 if '#' in name else 0) + 12 * (int(name[-1]) - 4)
    return 440 * 2 ** (semitone / 12)


def kick():
    x = seconds(0.35)
    pitch = 45 + 85 * np.exp(-x / 0.03)
    return np.sin(2 * np.pi * np.cumsum(pitch) / RATE) * np.exp(-x / 0.12) * np.minimum(1, x / 0.001)


NOISE = rng.standard_normal(RATE)
SNARE_NOISE = band(NOISE, 1200, 9000)
HAT_NOISE = band(NOISE, 7000, 16000)


def snare():
    x = seconds(0.25)
    body = np.sin(2 * np.pi * 190 * x) * np.exp(-x / 0.05)
    rattle = SNARE_NOISE[:len(x)] / np.abs(SNARE_NOISE).max() * np.exp(-x / 0.07)
    return 0.45 * body + 0.9 * rattle


def hat(open_=False):
    x = seconds(0.3 if open_ else 0.08)
    return HAT_NOISE[:len(x)] / np.abs(HAT_NOISE).max() * np.exp(-x / (0.09 if open_ else 0.018))


def pluck(freq, length, partials=10, decay=1.0):
    x = seconds(length)
    out = sum(np.sin(2 * np.pi * freq * k * x) / k ** 1.4 * np.exp(-x * decay * (1.5 + 0.8 * k))
              for k in range(1, partials + 1) if freq * k < 18000)
    return out * np.minimum(1, x / 0.004)


def pad(freqs, length):
    x = seconds(length)
    out = np.zeros(len(x))
    for f in freqs:
        for detune in (0.998, 1.002):
            out += sum(np.sin(2 * np.pi * f * detune * k * x + k) / k * np.exp(-k / 4) for k in range(1, 9))
    envelope = np.minimum(1, x / 0.25) * np.minimum(1, (length - x) / 0.3)
    return out * envelope


def bell(freq, length=1.6):
    x = seconds(length)
    out = sum(level * np.sin(2 * np.pi * freq * ratio * x) * np.exp(-x / decay)
              for ratio, level, decay in ((1, 1.0, 0.8), (2.76, 0.45, 0.4), (5.4, 0.3, 0.2), (8.93, 0.2, 0.1))
              if freq * ratio < 18000)
    return out * np.minimum(1, x / 0.002)


def place(track, sound, at):
    start = int(round(at * RATE))
    end = min(len(track), start + len(sound))
    track[start:end] += sound[:end - start]


def music(stems=False):
    """The mix; with stems=True instead each instrument alone, scaled and in
    the room exactly as in the mix, so that the six add up to it."""
    beat = 0.5                                   # 120 beats per minute
    length = 16 * beat
    n = int(length * RATE)
    kicks, snares, hats, bass, chords, melody = (np.zeros(n) for _ in range(6))
    for b in range(16):
        at = b * beat
        if b % 4 in (0, 2):
            place(kicks, kick(), at)
        if b % 4 in (1, 3):
            place(snares, 0.7 * snare(), at)
        place(hats, 0.32 * hat(), at)
        place(hats, 0.26 * hat(open_=(b % 2 == 1)), at + beat / 2)
    place(kicks, 0.8 * kick(), 15.5 * beat)
    drums = kicks + snares + hats
    progression = [('A2', ['A3', 'C4', 'E4']), ('F2', ['F3', 'A3', 'C4']),
                   ('C2', ['C4', 'E4', 'G4']), ('G2', ['G3', 'B3', 'D4'])]
    for bar, (root, triad) in enumerate(progression):
        place(chords, pad([note(t) for t in triad], 4 * beat + 0.2), bar * 4 * beat)
        for eighth in range(8):
            octave = 2 if eighth in (3, 6) else 1
            place(bass, pluck(note(root) * octave, 0.45, partials=6, decay=2.5), bar * 4 * beat + eighth * beat / 2)
    tune = [('E5', 0), ('G5', 1), ('A5', 1.5), ('C6', 2.5), ('A5', 3),
            ('C6', 4), ('A5', 5), ('G5', 5.5), ('E5', 6.5), ('D5', 7),
            ('E5', 8), ('G5', 9), ('C6', 9.5), ('D6', 10.5), ('C6', 11),
            ('B5', 12), ('G5', 13), ('D5', 13.5), ('E5', 14.5)]
    for name, at in tune:
        place(melody, bell(note(name)), at * beat)
    norm = lambda s: s / np.abs(s).max()
    dry = 0.62 * norm(drums) + 0.42 * norm(bass) + 0.2 * norm(chords) + 0.24 * norm(melody)
    # a small room: a short echo of noise, fading in 0,4 s
    m = int(0.6 * RATE)
    impulse = rng.standard_normal(m) * np.exp(-np.arange(m) / RATE / 0.12)
    impulse[0] = 0
    room = np.fft.irfft(np.fft.rfft(dry, n + m) * np.fft.rfft(impulse, n + m), n + m)[:n]
    wet = dry + 0.12 * room / np.abs(room).max() * np.abs(dry).max()
    fade = int(0.05 * RATE)
    wet[-fade:] *= np.linspace(1, 0, fade)
    if not stems:
        return wet / np.abs(wet).max() * 0.89
    # each instrument through the same scaling, room and fade as the mix
    scale = 0.89 / np.abs(wet).max()
    echo = 0.12 / np.abs(room).max() * np.abs(dry).max()
    parts = {'kick': 0.62 * kicks / np.abs(drums).max(), 'snare': 0.62 * snares / np.abs(drums).max(),
             'hi-hat': 0.62 * hats / np.abs(drums).max(), 'bass': 0.42 * norm(bass),
             'chords': 0.2 * norm(chords), 'melody': 0.24 * norm(melody)}
    out = {}
    for name, part in parts.items():
        part_room = np.fft.irfft(np.fft.rfft(part, n + m) * np.fft.rfft(impulse, n + m), n + m)[:n]
        part_wet = part + echo * part_room
        part_wet[-fade:] *= np.linspace(1, 0, fade)
        out[name] = part_wet * scale
    return out


def thorsten(sentence):
    from piper import PiperVoice
    voice = PiperVoice.load('/voices/de_DE-thorsten-high.onnx')
    audio = np.concatenate([np.asarray(c.audio_float_array, dtype=np.float32) for c in voice.synthesize(sentence)])
    audio = soxr.resample(audio.astype(np.float64), voice.config.sample_rate, RATE)
    pause = np.zeros(int(0.25 * RATE))
    return np.concatenate([pause, audio / np.abs(audio).max() * 0.89, pause])


PHONE = 8000
ROWS, COLUMNS = [697, 770, 852, 941], [1209, 1336, 1477]
KEYS = ['123', '456', '789', '*0#']


def dialling(number):
    """Each key two sines of 0,25, 0,12 to 0,18 s long with ramps of
    5 ms, pauses of 0,08 to 0,3 s, a quiet noise under all of it."""
    rng = np.random.default_rng(23)
    parts = [np.zeros(int(0.3 * PHONE))]
    for digit in number:
        row = next(i for i, keys in enumerate(KEYS) if digit in keys)
        t = np.arange(int(rng.uniform(0.12, 0.18) * PHONE)) / PHONE
        tone = 0.25 * np.sin(2 * np.pi * ROWS[row] * t) + 0.25 * np.sin(2 * np.pi * COLUMNS[KEYS[row].index(digit)] * t)
        ramp = np.minimum(1, np.minimum(t, t[-1] - t) / 0.005)
        parts += [tone * ramp, np.zeros(int(rng.uniform(0.08, 0.3) * PHONE))]
    signal = np.concatenate(parts + [np.zeros(int(0.3 * PHONE))])
    return signal + 0.02 * rng.standard_normal(len(signal))


def castanets():
    """Clicks: noise that dies away in 0,4 ms and a ring at 2.800 Hz
    that dies away in 1,5 ms; under them A minor, quietly."""
    rng = np.random.default_rng(31)
    t = np.arange(int(2.0 * RATE)) / RATE
    signal = sum(0.012 * np.sin(2 * np.pi * f * t) for f in (220.0, 261.63, 329.63))
    for at in (0.2, 0.35, 0.4, 0.45, 0.8, 1.2, 1.35, 1.4, 1.45, 1.8):
        i = int(at * RATE)
        u = np.arange(int(0.012 * RATE)) / RATE
        click = rng.uniform(-1, 1, len(u)) * np.exp(-u / 0.0004) + 0.5 * np.sin(2 * np.pi * 2800 * u) * np.exp(-u / 0.0015)
        signal[i:i + len(u)] += click
    fade = int(0.05 * RATE)
    signal[:fade] *= np.linspace(0, 1, fade)
    signal[-fade:] *= np.linspace(1, 0, fade)
    return signal / np.abs(signal).max() * 0.8


def main():
    os.makedirs(OUT, exist_ok=True)
    start = rng.bit_generator.state
    chosen = sys.argv[1:] or ['stimme', 'musik', 'waehlen', 'kastagnetten']
    if 'stimme' in chosen:
        write_wav(os.path.join(OUT, 'stimme.wav'), thorsten('Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich.'))
    if 'musik' in chosen:
        write_wav(os.path.join(OUT, 'musik.wav'), music())
    if 'spuren' in chosen:
        # the instruments of musik.wav one by one, as FLAC, for the switches
        # of the spectrogram widget (spectrum_widget.js); the random numbers
        # of snare, hi-hat and room are drawn as for musik.wav, from the
        # state the generator had when main began
        rng.bit_generator.state = start
        folder = os.path.join(OUT, 'musik')
        os.makedirs(folder, exist_ok=True)
        for name, part in music(stems=True).items():
            temporary = os.path.join(folder, f'{name}.wav')
            write_wav(temporary, part)
            subprocess.run(['ffmpeg', '-v', 'error', '-y', '-i', temporary, '-c:a', 'flac', os.path.join(folder, f'{name}.flac')], check=True)
            os.remove(temporary)
    if 'waehlen' in chosen:
        write_wav(os.path.join(OUT, 'waehlen.wav'), dialling('03023125386'), PHONE)
    if 'kastagnetten' in chosen:
        write_wav(os.path.join(OUT, 'kastagnetten.wav'), castanets())


main()
