FROM neo4j:2026.10.2-community@sha256:4a2fd4f41ce384aa7d6a74b399f22c202e154f3966119c28a18ed9a500904f5e

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y tzdata && rm -rf /var/cache/apt/*
RUN cp /usr/share/zoneinfo/Europe/Berlin /etc/localtime && echo "Europe/Berlin" > /etc/timezone
USER 1000:1000
