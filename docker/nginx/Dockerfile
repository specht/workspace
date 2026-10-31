FROM nginx:1.30.4@sha256:d5792f71a9496b833bc08ea834a758c46e2b6a6306c10f4be926f38a656cdc1c

# The Workspace proxy injects the browser-visible webfont stylesheet into code-server HTML.
RUN nginx -V 2>&1 | grep -q -- '--with-http_sub_module'

ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# 4096 connections per worker instead of 1024: a proxied request takes two,
# and one address may hold up to NGINX_CONNECTIONS_PER_CLIENT (main.rb).
# The workers may open as many files (the container's own limit is 1024).
RUN sed -i 's/worker_connections  1024;/worker_connections  4096;/; s/^worker_processes  auto;/worker_processes  auto;\nworker_rlimit_nofile 16384;/' /etc/nginx/nginx.conf \
    && grep -q 'worker_connections  4096;' /etc/nginx/nginx.conf && grep -q '^worker_rlimit_nofile 16384;' /etc/nginx/nginx.conf
