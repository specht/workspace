FROM debian:trixie-slim@sha256:a72b4433de1cf7d6bae0c4cee74d0547f58601a643b3375f070c51600d63b6b2

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
