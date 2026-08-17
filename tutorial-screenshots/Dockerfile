FROM mcr.microsoft.com/playwright:v1.62.1-noble

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        webp \
        fonts-ibm-plex \
        fonts-noto-core \
    && rm -rf /var/lib/apt/lists/*

COPY fontconfig-slight-hinting.conf /etc/fonts/conf.d/99-tutorial-screenshot-hinting.conf

WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci --omit=dev
COPY server.js ./server.js

CMD ["node", "server.js"]
