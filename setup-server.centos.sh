#!/bin/bash
set -e

# Achtung: Hier bitte den gewünschten Login und Public Key eintragen
LOGIN=""
PUBLIC_KEY=""

if [[ -z "$LOGIN" || -z "$PUBLIC_KEY" ]]; then
  echo "Fehler: Bitte zuerst Werte für LOGIN und PUBLIC_KEY eintragen." >&2
  exit 1
fi

# Add user
useradd -m -s /bin/bash "$LOGIN"
usermod -aG wheel "$LOGIN"
echo "$LOGIN ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/"$LOGIN"
chmod 440 /etc/sudoers.d/"$LOGIN"
mkdir -p /home/"$LOGIN"/.ssh
echo "$PUBLIC_KEY" > /home/"$LOGIN"/.ssh/authorized_keys
chmod 700 /home/"$LOGIN"/.ssh
chmod 600 /home/"$LOGIN"/.ssh/authorized_keys
chown -R "$LOGIN":"$LOGIN" /home/"$LOGIN"/.ssh

# Harden SSH
sed -i -e '/^\(#\|\)PermitRootLogin/s/^.*$/PermitRootLogin no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)PasswordAuthentication/s/^.*$/PasswordAuthentication no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)ChallengeResponseAuthentication/s/^.*$/ChallengeResponseAuthentication no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)MaxAuthTries/s/^.*$/MaxAuthTries 2/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)AllowTcpForwarding/s/^.*$/AllowTcpForwarding no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)X11Forwarding/s/^.*$/X11Forwarding no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)AllowAgentForwarding/s/^.*$/AllowAgentForwarding no/' /etc/ssh/sshd_config
sed -i -e '/^\(#\|\)AuthorizedKeysFile/s/^.*$/AuthorizedKeysFile .ssh\/authorized_keys/' /etc/ssh/sshd_config
systemctl reload sshd

# Update packages, enable EPEL
dnf install -y epel-release
dnf config-manager --set-enabled epel
dnf update -y

# Install VDO
dnf install -y vdo lvm2

# Install fail2ban
dnf install -y fail2ban
systemctl enable --now fail2ban
bash -c "echo -e '[sshd]\\nenabled=true\\nport=ssh\\nlogpath=%(sshd_log)s\\nbackend=systemd\\nmaxretry=5\\nbantime=1h\\nfindtime=10m' > /etc/fail2ban/jail.d/sshd.local"
systemctl restart fail2ban

# Install firewall
dnf install -y firewalld
systemctl enable --now firewalld
firewall-cmd --permanent --add-service=ssh
firewall-cmd --reload

# Install ruby, git, htop and bash-completion
dnf install -y ruby git htop bash-completion

# Install Docker CE
dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo
dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
systemctl enable --now docker
usermod -aG docker "$LOGIN"

# Install borgmatic
dnf install -y borgbackup python3-pip python3-setuptools
pip3 install --upgrade pip
pip3 install borgmatic

# Set timezone
timedatectl set-timezone Europe/Berlin

echo "Setup complete. You might want to reboot now."