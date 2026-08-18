FROM nginx:1.30.4

# The Workspace proxy injects the browser-visible webfont stylesheet into code-server HTML.
RUN nginx -V 2>&1 | grep -q -- '--with-http_sub_module'

ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
