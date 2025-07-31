#!/bin/bash
set -e

echo "Installiere Ruby..."
dnf install -yq ruby
echo "Hole nächstes Skript: 02-setup-server.rb"
wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/02-setup-server.rb -O 02-setup-server.rb
wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/common.rb -O common.rb
wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/config.yaml -O config.yaml
chmod +x 02-setup-server.rb
echo "Fertig."
