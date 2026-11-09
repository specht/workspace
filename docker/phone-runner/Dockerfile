# The runner of the Telefonspiel (docs/plan-telefonspiel.md): controls the
# calls through ARI, mixes music and speech live into each call (RTP),
# starts one game process per call. Speech: Piper, German voice Thorsten.
FROM python:3.12-slim
RUN apt-get update && apt-get install -y --no-install-recommends ffmpeg ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*
RUN pip install --no-cache-dir piper-tts==1.3.0 aiohttp==3.10.10 numpy==2.1.3 soxr==0.5.0.post1 yt-dlp==2025.10.22 G722==1.2.8
# the voice: Thorsten (Thorsten-Voice), high quality, from the Piper voices
RUN mkdir -p /voices && cd /voices \
    && curl -sSfL -o de_DE-thorsten-high.onnx https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten/high/de_DE-thorsten-high.onnx \
    && curl -sSfL -o de_DE-thorsten-high.onnx.json https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/de/de_DE/thorsten/high/de_DE-thorsten-high.onnx.json
RUN useradd -m -u 1500 runner && useradd -m -u 1501 game
WORKDIR /app
CMD ["python3", "-u", "/app/runner.py"]
