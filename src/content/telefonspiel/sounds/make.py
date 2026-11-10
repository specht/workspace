"""The sounds of »Ein Spiel am Telefon«, made here and not taken from
anywhere, so they are free to use: the music of the cave, which students
fetch into their Workspace (src/static/telefonspiel/hoehle.mp3), and the
listening examples of »Wie aus Ton Zahlen werden«: a sentence of Thorsten
over that music, at five sample rates. Runs in the image of the speech
service, which has numpy, soxr, ffmpeg and Thorsten:

    docker run --rm -u "$(id -u):$(id -g)" -v "$PWD:/repo" -w /repo \\
        --entrypoint python3 workspace_phone_speech src/content/telefonspiel/sounds/make.py

The music loops without a seam: every tone has a whole number of
oscillations in LOOP seconds, and the noise and the echo are filtered and
folded in a circle (FFT), so its end leads into its beginning.
"""

import os
import subprocess

import numpy as np
import soxr

HERE = os.path.dirname(os.path.abspath(__file__))
MUSIC = os.path.join(HERE, '..', '..', '..', 'static', 'telefonspiel', 'hoehle.mp3')
RATE = 44100
LOOP = 48                                   # seconds
N = RATE * LOOP
t = np.arange(N) / RATE
rng = np.random.default_rng(7)


def tone(freq):
    """a sine with a whole number of oscillations in the loop"""
    return np.sin(2 * np.pi * round(freq * LOOP) / LOOP * t)


def slow(cycles, phase=0.0):
    """0 … 1, cycles times in the loop"""
    return 0.5 + 0.5 * np.sin(2 * np.pi * cycles * t / LOOP + phase)


def band(signal, low, high):
    """only the frequencies between low and high, filtered in a circle"""
    spectrum = np.fft.rfft(signal)
    freqs = np.fft.rfftfreq(len(signal), 1 / RATE)
    spectrum[(freqs < low) | (freqs > high)] = 0
    return np.fft.irfft(spectrum, len(signal))


def place(out, sound, at):
    """sound into out from second at, wrapping round the end"""
    start = int(at * RATE)
    idx = (start + np.arange(len(sound))) % len(out)
    np.add.at(out, idx, sound)


def drip(freq):
    n = int(0.25 * RATE)
    x = np.arange(n) / RATE
    pitch = freq * (1 + 0.9 * (1 - np.exp(-x / 0.012)))          # rises quickly, as a drop does
    phase = 2 * np.pi * np.cumsum(pitch) / RATE
    return np.sin(phase) * np.exp(-x / 0.045) * np.minimum(1, x / 0.002)


def bell(freq, seconds=3.0):
    """a glass bell: a few partials, high ones fading faster"""
    n = int(seconds * RATE)
    x = np.arange(n) / RATE
    out = np.zeros(n)
    for ratio, level, decay in ((1, 1.0, 1.2), (2.76, 0.5, 0.6), (5.4, 0.35, 0.3), (8.93, 0.25, 0.15)):
        if freq * ratio < 20000:
            out += level * np.sin(2 * np.pi * freq * ratio * x) * np.exp(-x / decay)
    return out * np.minimum(1, x / 0.003)


def pluck(freq, seconds=2.5):
    n = int(seconds * RATE)
    x = np.arange(n) / RATE
    out = sum(np.sin(2 * np.pi * freq * k * x) / k ** 1.6 * np.exp(-x * (0.9 + 0.7 * k)) for k in range(1, 9))
    return out * np.minimum(1, x / 0.01)


def music():
    # the drone: A and E low down, a second A a quarter of a hertz higher, so it beats
    drone = (0.5 * tone(55) * (0.6 + 0.4 * slow(3)) + 0.35 * tone(82.5) * (0.5 + 0.5 * slow(2, 1.0))
             + 0.25 * (tone(110) + tone(110.25)) * (0.4 + 0.6 * slow(4, 2.0)))
    wind = band(rng.standard_normal(N), 250, 1400) * (0.25 + 0.75 * slow(2, 0.5) * slow(3, 1.7))
    air = band(rng.standard_normal(N), 7000, 16000) * (0.3 + 0.7 * slow(1, 2.5))    # what a low rate loses first
    notes = np.zeros(N)
    melody = [220, 261.63, 329.63, 293.66, 220, 196, 220, 164.81, 220, 261.63, 392, 329.63]
    for i, f in enumerate(melody):
        place(notes, pluck(f), i * LOOP / len(melody))
    drips = np.zeros(N)
    for at in sorted(rng.uniform(0, LOOP, 26)):
        place(drips, drip(rng.uniform(900, 1700)), at)
    bells = np.zeros(N)
    for at, f in ((5, 2637), (17, 3136), (29, 2349), (41, 3520)):
        place(bells, bell(f), at)
    dry = (0.55 * drone / np.abs(drone).max() + 0.07 * wind / np.abs(wind).max()
           + 0.018 * air / np.abs(air).max() + 0.35 * notes / np.abs(notes).max()
           + 0.30 * drips / np.abs(drips).max() + 0.18 * bells / np.abs(bells).max())
    # the cave: an echo two seconds long, folded in a circle
    n = int(2.0 * RATE)
    impulse = rng.standard_normal(n) * np.exp(-np.arange(n) / RATE / 0.45)
    impulse[0] = 0
    room = np.fft.irfft(np.fft.rfft(dry) * np.fft.rfft(impulse, N), N)
    wet = 0.75 * dry + 0.25 * room / np.abs(room).max() * np.abs(dry).max()
    return wet / np.abs(wet).max() * 0.89


def encode(samples, rate, path, bitrate):
    pcm = (np.clip(samples, -1, 1) * 32767).astype('<i2').tobytes()
    data = subprocess.run(['ffmpeg', '-loglevel', 'error', '-f', 's16le', '-ar', str(rate), '-ac', '1', '-i', '-',
                           '-c:a', 'libmp3lame', '-b:a', bitrate, '-cutoff', '20000', '-map_metadata', '-1',
                           '-fflags', '+bitexact', '-flags:a', '+bitexact', '-f', 'mp3', '-'],
                          input=pcm, check=True, capture_output=True).stdout
    with open(path, 'wb') as f:
        f.write(data)
    print(path, len(data), 'bytes')


def thorsten(sentence):
    from piper import PiperVoice
    voice = PiperVoice.load('/voices/de_DE-thorsten-high.onnx')
    audio = np.concatenate([np.asarray(c.audio_float_array, dtype=np.float32) for c in voice.synthesize(sentence)])
    return soxr.resample(audio / np.abs(audio).max(), voice.config.sample_rate, RATE)


def main():
    cave = music()
    os.makedirs(os.path.dirname(MUSIC), exist_ok=True)
    encode(cave, RATE, MUSIC, '96k')
    # the listening examples: music alone for a moment, then the sentence over it
    voice = thorsten('Sieben Fledermäuse schlafen still an der feuchten Felswand.')
    lead = int(1.5 * RATE)
    clip = cave[:lead + len(voice) + int(1.2 * RATE)].copy()
    under = np.ones(len(clip))
    under[lead - int(0.1 * RATE):lead + len(voice)] = 0.35         # quieter while Thorsten speaks, as on the phone
    under = np.convolve(under, np.ones(4000) / 4000, mode='same')
    clip = clip * under * 0.8
    clip[lead:lead + len(voice)] += 0.8 * voice
    fade = int(0.4 * RATE)
    clip[-fade:] *= np.linspace(1, 0, fade)
    clip /= np.abs(clip).max() / 0.89
    for rate in (44100, 16000, 8000, 4000, 2000):
        # measured at rate, then played back: what the rate keeps (soxr filters like a recorder)
        heard = clip if rate == RATE else soxr.resample(soxr.resample(clip, RATE, rate), rate, RATE)
        encode(heard, RATE, os.path.join(HERE, f'abtastung-{rate}.mp3'), '192k')


main()
