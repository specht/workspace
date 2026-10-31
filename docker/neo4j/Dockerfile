FROM neo4j:2026.07.1-community@sha256:956faa5d14b6beaa629b80c718bfd389ec12064f97e276ed9a58b0c53b25a323

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y tzdata && rm -rf /var/cache/apt/*
RUN cp /usr/share/zoneinfo/Europe/Berlin /etc/localtime && echo "Europe/Berlin" > /etc/timezone
USER 1000:1000
