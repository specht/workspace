FROM debian:bookworm-slim

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
