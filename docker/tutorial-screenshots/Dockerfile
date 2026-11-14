FROM mcr.microsoft.com/playwright:v1.65.1-noble@sha256:1507cbfc550a63a6b7396791ca4f00df17beb56342d2447b3f106c306f525579

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
