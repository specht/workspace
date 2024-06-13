FROM ruby:3-alpine

RUN apk update && apk upgrade
RUN apk add less curl bash curl-dev ruby-dev build-base docker tzdata libwebp imagemagick && rm -rf /var/cache/apk/*
RUN cp /usr/share/zoneinfo/Europe/Berlin /etc/localtime && echo "Europe/Berlin" > /etc/timezone
RUN gem install bundler
ADD Gemfile /
RUN bundle install

# RUN adduser -D -u 1000 hackschule
# RUN adduser hackschule docker
# RUN touch /var/run/docker.sock && chmod 666 /var/run/docker.sock
# USER hackschule
