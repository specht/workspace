FROM ruby:3.2-alpine

RUN adduser -D appuser
USER appuser

WORKDIR /workspace

COPY executor.rb /usr/local/bin/hackschule_executor.rb

ENTRYPOINT ["ruby", "/usr/local/bin/hackschule_executor.rb"]