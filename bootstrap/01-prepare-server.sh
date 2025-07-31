#!/bin/bash
set -e

dnf install -y ruby
wget https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/02-setup-server.rb
chmod +x 02-setup-server.rb