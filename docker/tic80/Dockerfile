ARG EMSCRIPTEN_VERSION=6.0.6
# the digest of that tag (src/scripts/image-digest.sh emscripten/emsdk:6.0.6)
ARG EMSDK_DIGEST=sha256:be96eff5810e42c632f3f8b795388a6b596e4fb21ec28b9e1fb1bc49bb3b1eef
FROM emscripten/emsdk:${EMSCRIPTEN_VERSION}@${EMSDK_DIGEST}

RUN apt-get update \
    && apt-get install -y --no-install-recommends ruby rake gcc-multilib \
    && rm -rf /var/lib/apt/lists/*