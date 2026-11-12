"""The signal of a German line before »Kein Anschluss unter dieser Nummer«
(the special information tone: 950, 1400 and 1800 Hz, a third of a second
each), written once as kein-anschluss.wav beside the menu, which plays it.
    python3 src/phone/line/tones.py"""
import math
import os
import struct
import wave

RATE = 16000
HERE = os.path.dirname(os.path.abspath(__file__))


def tone(frequency, seconds, volume=0.35):
    n = int(RATE * seconds)
    fade = int(RATE * 0.01)
    out = []
    for i in range(n):
        ramp = min(1.0, i / fade, (n - i) / fade)
        out.append(int(32767 * volume * ramp * math.sin(2 * math.pi * frequency * i / RATE)))
    return out


def silence(seconds):
    return [0] * int(RATE * seconds)


samples = []
for frequency in (950, 1400, 1800):
    samples += tone(frequency, 0.33) + silence(0.03)
samples += silence(0.5)
with wave.open(os.path.join(HERE, 'kein-anschluss.wav'), 'wb') as f:
    f.setnchannels(1)
    f.setsampwidth(2)
    f.setframerate(RATE)
    f.writeframes(struct.pack(f'<{len(samples)}h', *samples))
