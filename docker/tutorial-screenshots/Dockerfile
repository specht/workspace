FROM mcr.microsoft.com/playwright:v1.62.1-noble

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        webp \
        git \
        fonts-ibm-plex \
        fonts-noto-core \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci --omit=dev
COPY server.js ./server.js
COPY crop.js ./crop.js
COPY workspace-reset.js ./workspace-reset.js
COPY fontconfig-rendering.conf /etc/fonts/conf.d/99-tutorial-screenshot-rendering.conf

CMD ["node", "server.js"]
