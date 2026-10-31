FROM mcr.microsoft.com/playwright:v1.62.1-noble@sha256:dcc5531e97840b9b5e794f2814476b21571c5124a3fca2267d73041f56e7580e

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        webp \
        ffmpeg \
        git \
        graphviz \
        poppler-utils \
        imagemagick \
        fonts-ibm-plex \
        fonts-noto-core \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci --omit=dev
COPY server.js ./server.js
COPY crop.js ./crop.js
COPY plan.js ./plan.js
COPY git-graph.js ./git-graph.js
COPY workspace-reset.js ./workspace-reset.js
COPY includes.js ./includes.js
COPY fontconfig-rendering.conf /etc/fonts/conf.d/99-tutorial-screenshot-rendering.conf

CMD ["node", "server.js"]
