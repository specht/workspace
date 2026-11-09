"""The audio of one call, mixed live: music that loops, speech on top, the
music ducked under the speech. Fifty frames a second of 16 kHz audio (320
samples each) go to Asterisk as RTP, whether the game speaks, waits for
keys or does nothing.

The frames are sent as G.722 (payload type 9), encoded here: Asterisk 20
does not take 16 kHz linear audio back from External Media (neither over
RTP with a dynamic payload type nor over AudioSocket, which it reads as
8 kHz), but it takes G.722, and so the audio keeps its 16 kHz. G.722's RTP
clock runs at 8000 per second (RFC 3551), so the timestamp grows by 160
per frame."""

import asyncio
import os
import random
import struct

import numpy as np
from G722 import G722

RATE = 16000
FRAME = RATE // 50                 # 20 ms
DUCK = 0.25                        # the music under speech, of its volume
ATTACK = 0.08                      # seconds for the music to duck
RELEASE = 0.4                      # seconds to come back after speech


class Mixer:
    def __init__(self, sock, remote):
        self.sock = sock
        self.remote = remote       # where Asterisk wants our RTP
        self.encoder = G722(RATE, 64000)
        self.seq = random.randrange(65536)
        self.timestamp = random.randrange(1 << 32)
        self.ssrc = random.randrange(1 << 32)
        self.music = None          # float32 array, looped
        self.music_pos = 0
        self.music_volume = 0.5
        self.duck_gain = 1.0
        self.speech = []           # queue of [float32 array, position, done future]
        self.task = None

    def start(self):
        self.task = asyncio.get_running_loop().create_task(self._run())

    def stop(self):
        if self.task:
            self.task.cancel()
        for item in self.speech:
            if not item[2].done():
                item[2].set_result(False)
        self.speech = []

    def set_music(self, samples, volume=0.5):
        """samples: float32 at 16 kHz, or None to stop the music"""
        self.music = samples
        self.music_pos = 0
        self.music_volume = max(0.0, min(1.0, float(volume)))

    def say(self, samples):
        """queue speech; the future is done when it has been heard"""
        done = asyncio.get_running_loop().create_future()
        self.speech.append([samples, 0, done])
        return done

    def speaking(self):
        return bool(self.speech)

    def _next_frame(self):
        out = np.zeros(FRAME, dtype=np.float32)
        # speech first: its frame, and whether there is any
        voice = np.zeros(FRAME, dtype=np.float32)
        filled = 0
        while self.speech and filled < FRAME:
            samples, pos, done = self.speech[0]
            take = min(FRAME - filled, len(samples) - pos)
            voice[filled:filled + take] = samples[pos:pos + take]
            filled += take
            pos += take
            if pos >= len(samples):
                self.speech.pop(0)
                if not done.done():
                    done.set_result(True)
            else:
                self.speech[0][1] = pos
        # ducking: a smooth ramp towards DUCK while speech plays, back to 1
        target = DUCK if (filled > 0 or self.speech) else 1.0
        seconds = ATTACK if target < self.duck_gain else RELEASE
        step = (1.0 - DUCK) * FRAME / (RATE * seconds)
        start = self.duck_gain
        if target < start:
            end = max(target, start - step)
        else:
            end = min(target, start + step)
        self.duck_gain = end
        if self.music is not None and len(self.music):
            idx = (self.music_pos + np.arange(FRAME)) % len(self.music)
            self.music_pos = (self.music_pos + FRAME) % len(self.music)
            ramp = np.linspace(start, end, FRAME, dtype=np.float32)
            out += self.music[idx] * self.music_volume * ramp
        out += voice
        return np.clip(out, -1.0, 1.0)

    def _packet(self, frame):
        payload = self.encoder.encode((frame * 32767).astype(np.int16))
        header = struct.pack('!BBHII', 0x80, 9, self.seq, self.timestamp, self.ssrc)
        self.seq = (self.seq + 1) & 0xFFFF
        self.timestamp = (self.timestamp + FRAME // 2) & 0xFFFFFFFF
        return header + bytes(payload)

    async def _run(self):
        loop = asyncio.get_running_loop()
        tick = loop.time()
        while True:
            frame = self._next_frame()
            try:
                self.sock.sendto(self._packet(frame), self.remote)
            except OSError:
                pass
            tick += FRAME / RATE
            delay = tick - loop.time()
            if delay < -0.2:            # far behind (paused machine): catch up
                tick = loop.time()
            elif delay > 0:
                await asyncio.sleep(delay)


def pcm16_to_float(data, rate):
    """int16 bytes at `rate` to float32 at 16 kHz"""
    import soxr
    samples = np.frombuffer(data, dtype=np.int16).astype(np.float32) / 32768.0
    if rate != RATE:
        samples = soxr.resample(samples, rate, RATE)
    return samples.astype(np.float32)


def silence(ms):
    return np.zeros(int(RATE * ms / 1000), dtype=np.float32)


if os.environ.get('MIXER_SELFTEST'):
    m = Mixer(None, None)
    m.set_music(np.ones(RATE, dtype=np.float32) * 0.5, 1.0)
    print([round(float(np.abs(m._next_frame()).mean()), 3) for _ in range(3)])
