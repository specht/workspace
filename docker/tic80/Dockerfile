FROM emscripten/emsdk
RUN sudo apt-get update && sudo apt-get install -y \
    rake jsbeautifier \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*
