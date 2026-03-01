FROM node:20-alpine

RUN adduser -D appuser
USER appuser

WORKDIR /workspace

COPY executor.js /usr/local/bin/hackschule_executor.js

ENTRYPOINT ["node", "/usr/local/bin/hackschule_executor.js"]
