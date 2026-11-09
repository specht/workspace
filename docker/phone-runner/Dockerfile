# The runner of the Telefonspiel (docs/plan-telefonspiel.md): controls the
# calls through ARI, mixes music and speech live into each call (RTP),
# starts one game process per call (Python, Ruby or JavaScript). Speech
# comes from the speech service (docker/phone-speech). The code is mounted
# at /phone (src/phone), the games at /games.
FROM python:3.12-slim
RUN apt-get update && apt-get install -y --no-install-recommends ffmpeg ruby nodejs \
    && rm -rf /var/lib/apt/lists/*
RUN pip install --no-cache-dir aiohttp==3.10.10 numpy==2.1.3 G722==1.2.8
RUN useradd -m -u 1500 runner && useradd -m -u 1501 game
CMD ["python3", "-u", "/phone/runner/runner.py"]
