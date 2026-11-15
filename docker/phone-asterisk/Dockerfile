# Asterisk for the Telefonspiel (docs/plan-telefonspiel.md): answers calls
# from a SIP trunk or a test telephone and hands each to the runner (ARI).
# Alpine 3.23 for Asterisk 20 (LTS): with Asterisk 22 (Alpine 3.24) real calls
# from the trunk got no audio either way (tried 14 November 2026: the menu
# played, the caller heard nothing, no key arrived), while the test telephone
# in src/phone/test/e2e.sh still worked
FROM alpine:3.23@sha256:469c9ea410b9215eaee7976ba6c0639039b444be8e5d638f7abbada189971eb2
RUN apk add --no-cache asterisk asterisk-srtp
# Nothing a caller says is recorded or kept: the folders where Asterisk
# would put recordings, voicemail and monitored calls belong to root and
# cannot be written by Asterisk (modules.conf turns the modules off).
RUN for d in recording monitor voicemail meetme dictate; do \
        rm -rf /var/spool/asterisk/$d && mkdir -p /var/spool/asterisk/$d && chown root:root /var/spool/asterisk/$d && chmod 555 /var/spool/asterisk/$d; \
    done
# the configuration comes from src/phone/asterisk, mounted read-only
CMD ["asterisk", "-f", "-U", "asterisk", "-G", "asterisk", "-C", "/etc/asterisk/asterisk.conf"]
