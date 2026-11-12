# Asterisk for the Telefonspiel (docs/plan-telefonspiel.md): answers calls
# from a SIP trunk or a test telephone and hands each to the runner (ARI).
FROM alpine:3.20
RUN apk add --no-cache asterisk asterisk-srtp
# Nothing a caller says is recorded or kept: the folders where Asterisk
# would put recordings, voicemail and monitored calls belong to root and
# cannot be written by Asterisk (modules.conf turns the modules off).
RUN for d in recording monitor voicemail meetme dictate; do \
        rm -rf /var/spool/asterisk/$d && mkdir -p /var/spool/asterisk/$d && chown root:root /var/spool/asterisk/$d && chmod 555 /var/spool/asterisk/$d; \
    done
# the configuration comes from src/phone/asterisk, mounted read-only
CMD ["asterisk", "-f", "-U", "asterisk", "-G", "asterisk", "-C", "/etc/asterisk/asterisk.conf"]
