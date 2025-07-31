#!/bin/bash
set -e

dnf install -y ruby
wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/02-setup-server.rb -O 02-setup-server.rb
wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/config.yaml -O config.yaml
chmod +x 02-setup-server.rb
