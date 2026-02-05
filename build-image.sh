#!/bin/bash
wget -N -O docker/code/dart.deb https://storage.googleapis.com/dart-archive/channels/stable/release/3.8.2/linux_packages/dart_3.8.2-1_amd64.deb
cp src/sidebar-init/hackschule-sidebar-init-0.0.1.vsix docker/code/hackschule-sidebar-init.vsix
cd docker/code && docker build -t hs_code_server .
