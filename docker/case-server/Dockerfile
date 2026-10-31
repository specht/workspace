# The case server of »Ein Krimi im Terminal« (docs/entwicklung.md, »Der
# Fallserver«): one image per host, ROLE museum or alarmanlage. The files of
# the case come from src/content/terminal-mystery/beispiele/make_case.rb.
FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3

ARG ROLE
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        openssh-server openssh-client openssl git less procps tzdata file nano \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /etc/ssh/ssh_host_* /etc/update-motd.d/* \
    && mkdir -p /run/sshd

ENV TZ=Europe/Berlin LANG=C.UTF-8

COPY files/${ROLE}/ /
# The alarm system's repository, unpacked; everything belongs to root and
# can be read by everyone, nobody but root can change it.
RUN if [ -f /alarmanlage.tar.gz ]; then tar xzf /alarmanlage.tar.gz -C /srv && rm /alarmanlage.tar.gz; fi \
    && chown -R root:root /srv /home \
    && chmod -R a+rX,go-w /srv /home \
    && if [ -f /etc/case-alarm-password ]; then chmod 0600 /etc/case-alarm-password; fi \
    && for f in /srv/museum/kameras/cam*_*.jpg; do \
           [ -f "$f" ] || continue; \
           t=${f##*_}; t=${t%.jpg}; d=${f%_*}; d=${d##*_}; \
           touch -d "$(echo $d | cut -c1-4)-$(echo $d | cut -c5-6)-$(echo $d | cut -c7-8) $(echo $t | cut -c1-2):$(echo $t | cut -c3-4):$(echo $t | cut -c5-6)" "$f"; \
       done \
    && cp /etc/passwd /etc/passwd.image && cp /etc/shadow /etc/shadow.image && cp /etc/group /etc/group.image

COPY case.sh /etc/profile.d/case.sh
COPY limits.conf /etc/security/limits.d/case.conf
COPY entrypoint.sh /usr/local/sbin/case-server
RUN chmod 0755 /usr/local/sbin/case-server \
    && printf '[safe]\n\tdirectory = *\n' > /etc/gitconfig

ENV ROLE=${ROLE}
EXPOSE 22
ENTRYPOINT ["/usr/local/sbin/case-server"]
