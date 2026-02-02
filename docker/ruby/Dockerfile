FROM ruby:3.2-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
  less \
  curl \
  bash \
  tzdata \
  sysstat \
  graphviz \
  build-essential \
  pkg-config \
  imagemagick \
  libwebp-dev \
  default-libmysqlclient-dev \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
  ca-certificates \
  curl \
  gnupg \
  && install -m 0755 -d /etc/apt/keyrings \
  && curl -fsSL https://download.docker.com/linux/debian/gpg \
     | gpg --dearmor -o /etc/apt/keyrings/docker.gpg \
  && chmod a+r /etc/apt/keyrings/docker.gpg \
  && echo \
     "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] \
     https://download.docker.com/linux/debian \
     bookworm stable" \
     > /etc/apt/sources.list.d/docker.list \
  && apt-get update \
  && apt-get install -y docker-ce-cli \
  && rm -rf /var/lib/apt/lists/*

ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN gem install bundler

WORKDIR /app
COPY Gemfile ./
RUN bundle install
