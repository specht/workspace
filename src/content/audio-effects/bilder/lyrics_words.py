"""The words of »Ode an die Freude« spoken by Thorsten (a free voice for
speech output, CC0, the voice of stimme.wav), one file per word at the
voice's own rate, for voice_melody.py. Runs in the speech service of the
Telefonspiel, which has Piper and the voice:

    docker exec -i workspace_phone_speech_1 python3 - < src/content/audio-effects/bilder/lyrics_words.py
    docker exec workspace_phone_speech_1 tar -C /tmp/lyrics -cf - . | tar -C src/content/audio-effects/bilder/lyrics -xf -
"""

import os
import wave

import numpy as np
from piper import PiperVoice

WORDS = ['Freude', 'schöner', 'Götterfunken', 'Tochter', 'aus', 'Elysium']
VOICE = os.environ.get('PHONE_VOICE', '/voices/de_DE-thorsten-high.onnx')
OUT = '/tmp/lyrics'

voice = PiperVoice.load(VOICE)
rate = voice.config.sample_rate
os.makedirs(OUT, exist_ok=True)
for k, word in enumerate(WORDS):
    ids = voice.phonemes_to_ids([p for piece in voice.phonemize(word) for p in piece])
    samples = np.asarray(voice.phoneme_ids_to_audio(ids), dtype=np.float32)
    samples /= max(1e-8, float(np.max(np.abs(samples))))
    name = word.lower().replace('ö', 'oe')
    with wave.open(os.path.join(OUT, f'{k}-{name}.wav'), 'wb') as f:
        f.setnchannels(1)
        f.setsampwidth(2)
        f.setframerate(rate)
        f.writeframes((samples * 0.9 * 32767).astype('<i2').tobytes())
    print(word, len(samples) / rate, 's at', rate)
