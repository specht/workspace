#!/bin/bash
curl -sL https://storage.googleapis.com/dart-archive/channels/stable/release/3.8.2/linux_packages/dart_3.8.2-1_amd64.deb -o docker/code/dart.deb
cd docker/code && docker build -t hs_code_server .
