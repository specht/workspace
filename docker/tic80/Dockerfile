ARG EMSCRIPTEN_VERSION=6.0.6
FROM emscripten/emsdk:${EMSCRIPTEN_VERSION}

RUN apt-get update \
    && apt-get install -y --no-install-recommends ruby rake \
    && rm -rf /var/lib/apt/lists/*