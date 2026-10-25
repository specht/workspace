FROM ruby:3.4.10-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
  less \
  curl \
  tar \
  unzip \
  wget \
  bzip2 \
  bash \
  tzdata \
  sysstat \
  qrencode \
  build-essential \
  pkg-config \
  imagemagick \
  libwebp-dev \
  weasyprint \
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

ARG BUNDLER_VERSION=4.0.17
RUN gem install bundler --version "${BUNDLER_VERSION}"

# Only for task_examples_test.rb: the steganography checks run the students'
# Ruby, which reads PNGs with chunky_png (the website itself does not need it).
RUN gem install chunky_png --version 1.4.0

WORKDIR /app
COPY Gemfile Gemfile.lock ./
RUN bundle config set frozen true \
  && bundle install
