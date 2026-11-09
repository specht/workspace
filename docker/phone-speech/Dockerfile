# The speech service of the Telefonspiel (src/phone/speech/server.py): Piper
# with the German voice Thorsten for every call and every emulator, and
# music from YouTube (yt-dlp), cached as files in /cache. The code is
# mounted at /app (src/phone/speech).
FROM python:3.12-slim
RUN apt-get update && apt-get install -y --no-install-recommends ffmpeg ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
# yt-dlp needs a JavaScript runtime for YouTube (Deno, pinned with it); an
# old yt-dlp gets 403 from YouTube, so update it when music fails
RUN pip install --no-cache-dir piper-tts==1.3.0 aiohttp==3.10.10 numpy==2.1.3 soxr==0.5.0.post1 \
    'yt-dlp[default,pin,pin-deno]==2026.10.26'
# the voice: Thorsten (Thorsten-Voice, CC0), high quality, and Thorsten in
# eight moods (a speaker each, say … mood:), from the Piper voices
RUN mkdir -p /voices && cd /voices \
    && curl -sSfL -o de_DE-thorsten-high.onnx https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten/high/de_DE-thorsten-high.onnx \
    && curl -sSfL -o de_DE-thorsten-high.onnx.json https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten/high/de_DE-thorsten-high.onnx.json \
    && curl -sSfL -o de_DE-thorsten_emotional-medium.onnx https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten_emotional/medium/de_DE-thorsten_emotional-medium.onnx \
    && curl -sSfL -o de_DE-thorsten_emotional-medium.onnx.json https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten_emotional/medium/de_DE-thorsten_emotional-medium.onnx.json
# Whisper for the review of submitted games: what the students' own
# recordings (play '….mp3') say, written out on the server, never sent
# elsewhere; the small multilingual model »base«, fetched here, not at run
RUN pip install --no-cache-dir faster-whisper==1.1.1 \
    && python3 -c "from faster_whisper import WhisperModel; WhisperModel('base', device='cpu', compute_type='int8', download_root='/voices/whisper')"
RUN useradd -m -u 1500 speech && mkdir -p /cache
# the cache folder (mounted from the host) to the user speech, then only as
# speech; the YouTube cookies (PHONE_YOUTUBE_COOKIES, readable only by the
# operator on the host) copied for speech first, which yt-dlp may rewrite
CMD ["sh", "-c", "chown speech:speech /cache && if [ -f /secrets/youtube-cookies.txt ]; then cp /secrets/youtube-cookies.txt /tmp/youtube-cookies.txt && chown speech:speech /tmp/youtube-cookies.txt && chmod 600 /tmp/youtube-cookies.txt; fi && exec setpriv --reuid speech --regid speech --clear-groups env HOME=/home/speech python3 -u /app/server.py"]
