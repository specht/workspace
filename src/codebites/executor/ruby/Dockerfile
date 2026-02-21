FROM ruby:3.2-alpine

# Create non-root user
RUN adduser -D appuser
USER appuser

WORKDIR /app

COPY executor.rb /app/executor.rb

ENTRYPOINT ["ruby", "/app/executor.rb"]