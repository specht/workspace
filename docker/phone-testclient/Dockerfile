# A SIP test telephone for the Telefonspiel: calls in, plays a file as its
# microphone, records what it hears, sends keys (baresip, src/phone/test)
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends baresip-core sox python3 \
    && rm -rf /var/lib/apt/lists/*
