FROM mcr.microsoft.com/playwright:v1.62.1-noble

RUN apt-get update \
    && apt-get install -y --no-install-recommends webp \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci --omit=dev
COPY server.js ./server.js

CMD ["node", "server.js"]
