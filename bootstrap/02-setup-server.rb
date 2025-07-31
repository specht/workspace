#!/usr/bin/env ruby

require 'yaml'
require 'io/console'
require './common.rb'

config = YAML.load(File.read('config.yaml'))

LOGIN = config['login']
PUBLIC_KEY = config['public_key']
DOMAIN = config['domain']
STORAGE_DEVICE = config['storage_device']

unless Process.uid == 0
    puts "Dieses Skript muss als root laufen."
    exit(1)
end

if LOGIN.nil? || PUBLIC_KEY.nil? || DOMAIN.nil? || STORAGE_DEVICE.nil?
    puts "Bevor es losgehen kann, musst du in der config.yaml ein paar Angaben machen:"
    puts
    puts "login          : Dein Login auf dem Server (nach außen nicht sichtbar)"
    puts "public_key     : Dein Public Key"
    puts "domain         : Die Domain, unter der Workspace gehostet werden soll"
    puts "storage_device : Der Pfad zum Volume (z. B. /dev/sdb)"
    puts
    exit(1)
end

puts colored(" ACHTUNG ", color: :white, bg: :red)
puts
puts "Dieses Skript nimmt umfangreiche Änderungen an diesem Server vor und"
puts "richtet den Hackschule Workspace ein. Führe dieses Skript nur auf einem"
puts "frischen Server aus (Cent OS Stream 10). Auf dem Server oder angeschlossenen"
puts "Volumes dürfen sich keine Daten befinden, die du noch brauchst."
puts
exit unless confirm("Bist du sicher, dass du mit der Einrichtung beginnen möchtest (j/n)?")

puts "Los geht's!"

puts colored(" Hackschule Workspace Servereinrichtung ", color: :white, bg: :blue, bold: true)

puts colored("1. Füge Nutzer hinzu: #{LOGIN} ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    useradd -m -s /bin/bash #{LOGIN}
    usermod -aG wheel #{LOGIN}
    echo "#{LOGIN} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/#{LOGIN}
    chmod 440 /etc/sudoers.d/#{LOGIN}
    mkdir -p /home/#{LOGIN}/.ssh
    echo "#{PUBLIC_KEY}" > /home/#{LOGIN}/.ssh/authorized_keys
    chmod 700 /home/#{LOGIN}/.ssh
    chmod 600 /home/#{LOGIN}/.ssh/authorized_keys
    chown -R #{LOGIN}:#{LOGIN} /home/#{LOGIN}/.ssh
END_OF_STRING

puts colored("2. Härte SSH-Server ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    sed -i -e '/^\(#\|\)PermitRootLogin/s/^.*$/PermitRootLogin no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)PasswordAuthentication/s/^.*$/PasswordAuthentication no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)ChallengeResponseAuthentication/s/^.*$/ChallengeResponseAuthentication no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)MaxAuthTries/s/^.*$/MaxAuthTries 2/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)AllowTcpForwarding/s/^.*$/AllowTcpForwarding no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)X11Forwarding/s/^.*$/X11Forwarding no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)AllowAgentForwarding/s/^.*$/AllowAgentForwarding no/' /etc/ssh/sshd_config
    sed -i -e '/^\(#\|\)AuthorizedKeysFile/s/^.*$/AuthorizedKeysFile .ssh\/authorized_keys/' /etc/ssh/sshd_config
    systemctl reload sshd
END_OF_STRING

puts colored("3. Paketupdates ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y epel-release
    dnf config-manager --set-enabled epel
    dnf update -y
END_OF_STRING

puts colored("4. Installiere VDO  ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y vdo lvm2
END_OF_STRING

puts colored("5. Installiere fail2ban ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y fail2ban
    systemctl enable --now fail2ban
    bash -c "echo -e '[sshd]\\nenabled=true\\nport=ssh\\nlogpath=%(sshd_log)s\\nbackend=systemd\\nmaxretry=5\\nbantime=1h\\nfindtime=10m' > /etc/fail2ban/jail.d/sshd.local"
    systemctl restart fail2ban
END_OF_STRING

puts colored("6. Installiere Firewall ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y firewalld
    systemctl enable --now firewalld
    firewall-cmd --permanent --add-service=ssh
    firewall-cmd --reload
END_OF_STRING

puts colored("7. Installiere Git, htop und bash-completion ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y git htop bash-completion
END_OF_STRING

puts colored("8. Installiere Docker ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo
    dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
    systemctl enable --now docker
    usermod -aG docker #{LOGIN}
    systemctl disable docker.service
END_OF_STRING

puts colored("9. Installiere borg und borgmatic ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    dnf install -y borgbackup python3-pip python3-setuptools
    pip3 install --upgrade pip
    pip3 install borgmatic
END_OF_STRING

puts colored("10. Setze Zeitzone ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    timedatectl set-timezone Europe/Berlin
END_OF_STRING

puts colored("11. Hole nächstes Skript: 03-setup-storage.rb ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/03-setup-storage.rb -O 03-setup-storage.rb
    chmod +x 03-setup-storage.rb
END_OF_STRING

puts colored("Fertig, starte nun den Server neu mit: reboot now", color: :green, bold: true)

# curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/01-prepare-server.sh | sh

