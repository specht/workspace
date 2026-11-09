"""The mixer alone (in the phone-runner image):
    docker run --rm -v $PWD/src/phone:/phone phone-runner python3 /phone/test/test_mixer.py
Music at a steady level, then a second of speech: the music ducks within
ATTACK, stays ducked while speech plays and comes back within RELEASE;
speech passes unchanged; a frame always has 320 samples."""
import asyncio
import sys
import unittest

import numpy as np

sys.path.insert(0, '/phone/runner')
from mixer import ATTACK, DUCK, FRAME, RATE, RELEASE, Mixer  # noqa: E402


class MixerTest(unittest.TestCase):
    def test_ducking(self):
        async def run():
            mixer = Mixer(None, None)
            mixer.set_music(np.full(RATE, 0.4, dtype=np.float32), volume=1.0)
            frames = [mixer._next_frame() for _ in range(25)]          # 0.5 s music alone
            done = mixer.say(np.zeros(RATE, dtype=np.float32))          # 1 s of (silent) speech
            frames += [mixer._next_frame() for _ in range(100)]
            self.assertTrue(done.done() and done.result())
            frames += [mixer._next_frame() for _ in range(50)]          # 1 s after it
            level = [float(np.mean(f)) for f in frames]
            self.assertTrue(all(len(f) == FRAME for f in frames))
            self.assertAlmostEqual(level[20], 0.4, places=3)           # before: full
            attack_frames = int(ATTACK * 50) + 1
            self.assertAlmostEqual(level[25 + attack_frames + 1], 0.4 * DUCK, places=3)   # ducked
            self.assertAlmostEqual(level[73], 0.4 * DUCK, places=3)   # still ducked near its end (frame 75)
            release_frames = int(RELEASE * 50) + 1
            self.assertAlmostEqual(level[75 + release_frames + 1], 0.4, places=3)         # back up
            ramp = level[75:75 + release_frames]
            self.assertTrue(all(b >= a for a, b in zip(ramp, ramp[1:])))                  # smoothly
        asyncio.run(run())

    def test_speech_passes_unchanged(self):
        async def run():
            mixer = Mixer(None, None)
            speech = np.linspace(-0.5, 0.5, FRAME * 3, dtype=np.float32)
            mixer.say(speech)
            out = np.concatenate([mixer._next_frame() for _ in range(3)])
            np.testing.assert_allclose(out, speech, atol=1e-6)
        asyncio.run(run())

    def test_g722_packet(self):
        mixer = Mixer(None, None)
        packet = mixer._packet(np.zeros(FRAME, dtype=np.float32))
        self.assertEqual(len(packet), 12 + FRAME // 2)                 # header + 160 bytes of G.722
        self.assertEqual(packet[1] & 0x7F, 9)                           # payload type 9
        first = int.from_bytes(packet[4:8], 'big')
        second = int.from_bytes(mixer._packet(np.zeros(FRAME, dtype=np.float32))[4:8], 'big')
        self.assertEqual((second - first) & 0xFFFFFFFF, 160)            # G.722's 8 kHz clock


unittest.main(verbosity=2)
