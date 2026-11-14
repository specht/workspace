# A SIP test telephone for the Telefonspiel: calls in, plays a file as its
# microphone, records what it hears, sends keys (baresip, src/phone/test)
FROM debian:trixie-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2
RUN apt-get update && apt-get install -y --no-install-recommends baresip-core sox python3 \
    && rm -rf /var/lib/apt/lists/*
