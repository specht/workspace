FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        jq \
        nftables \
    && rm -rf /var/lib/apt/lists/*

COPY peer-firewall.sh /usr/local/sbin/peer-firewall
RUN chmod 0755 /usr/local/sbin/peer-firewall

ENTRYPOINT ["/usr/local/sbin/peer-firewall"]
