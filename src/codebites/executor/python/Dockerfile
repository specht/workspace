FROM python:3.12-alpine

RUN adduser -D appuser

# Create workspace directory
RUN mkdir /workspace && chown appuser:appuser /workspace

USER appuser
WORKDIR /workspace

COPY executor.py /usr/local/bin/hackschule_executor.py

ENTRYPOINT ["python", "-u", "/usr/local/bin/hackschule_executor.py"]