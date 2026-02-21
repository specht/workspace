FROM python:3.12-alpine

RUN adduser -D appuser
USER appuser

WORKDIR /workspace

COPY executor.py /usr/local/bin/hackschule_executor.py

ENTRYPOINT ["python", "/usr/local/bin/hackschule_executor.py"]