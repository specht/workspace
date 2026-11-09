# Asterisk for the Telefonspiel (docs/plan-telefonspiel.md): answers calls
# from a SIP trunk or a test telephone and hands each to the runner (ARI).
FROM alpine:3.20
RUN apk add --no-cache asterisk asterisk-srtp
# the configuration comes from src/phone/asterisk, mounted read-only
CMD ["asterisk", "-f", "-U", "asterisk", "-G", "asterisk", "-C", "/etc/asterisk/asterisk.conf"]
