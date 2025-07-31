#!/usr/bin/env ruby
require 'yaml'
require 'io/console'

def colored(text, color: nil, bg: nil, bold: false, underline: false)
    codes = []

    colors = {
        black: 30, red: 31, green: 32, yellow: 33, blue: 34,
        magenta: 35, cyan: 36, white: 37, gray: 90
    }

    bg_colors = {
        black: 40, red: 41, green: 42, yellow: 43, blue: 44,
        magenta: 45, cyan: 46, white: 47
    }

    codes << colors[color.to_sym] if color && colors[color.to_sym]
    codes << bg_colors[bg.to_sym] if bg && bg_colors[bg.to_sym]
    codes << 1 if bold
    codes << 4 if underline

    start = "\e[#{codes.join(';')}m"
    reset = "\e[0m"

    "#{start}#{text}#{reset}"
end

def confirm(prompt = "Do you want to proceed? (j/n): ")
    print "#{prompt} "
    input = gets.chomp.strip

    until input.match?(/\A[Jn]\z/i)
        print "Bitte gib j (ja) oder n (nein) ein: "
        input = gets.chomp.strip
    end

    input.downcase == "j"
end

def run_with_scrolling_tail(cmd, line_limit: 8)
    lines = []
    IO.popen(cmd, err: [:child, :out]) do |io|
        io.each_line do |line|
            lines << line.chomp
            print "\e[#{lines.size - 1}A" unless lines.size == 1
            lines.shift if lines.size > line_limit
            lines.each { |l| puts l.ljust(IO.console.winsize[1]) }
            # sleep 0.1
                # puts line.ljust(IO.console.winsize[1]).strip
        end
    end
    # Process.wait
    status = $?.exitstatus
    # print "\e[#{lines.size}A"
    # lines.each { puts " " * IO.console.winsize[1] }
    # print "\e[#{lines.size}A"
    if status != 0
        puts "❌ Ups, etwas ging schief:\nBefehl: #{cmd}\nStatus: #{status}"
        exit status
    end
end

config = YAML.load(File.read('config.yaml'))

LOGIN = config['login']
PUBLIC_KEY = config['public_key']
DOMAIN = config['domain']

unless Process.uid == 0
    puts "Dieses Skript muss als root laufen."
    exit(1)
end

if LOGIN.nil? || PUBLIC_KEY.nil? || DOMAIN.nil?
    puts "Bevor es losgehen kann, musst du in der config.yaml ein paar Angaben machen:"
    puts
    puts "login      : Dein Login auf dem Server (nach außen nicht sichtbar)"
    puts "public_key : Dein Public Key"
    puts "domain     : Die Domain, unter der Workspace gehostet werden soll"
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
    echo "$PUBLIC_KEY" > /home/#{LOGIN}/.ssh/authorized_keys
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
    # Install VDO
    dnf install -y vdo lvm2
END_OF_STRING

puts colored("5. Installiere fail2ban ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Install fail2ban
    dnf install -y fail2ban
    systemctl enable --now fail2ban
    bash -c "echo -e '[sshd]\\nenabled=true\\nport=ssh\\nlogpath=%(sshd_log)s\\nbackend=systemd\\nmaxretry=5\\nbantime=1h\\nfindtime=10m' > /etc/fail2ban/jail.d/sshd.local"
    systemctl restart fail2ban
END_OF_STRING

puts colored("6. Installiere Firewall ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Install firewall
    dnf install -y firewalld
    systemctl enable --now firewalld
    firewall-cmd --permanent --add-service=ssh
    firewall-cmd --reload
END_OF_STRING

puts colored("7. Installiere Ruby, Git, htop und bash-completion ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Install ruby, git, htop and bash-completion
    dnf install -y ruby git htop bash-completion
END_OF_STRING

puts colored("8. Installiere Docker ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Install Docker CE
    dnf config-manager --add-repo=https://download.docker.com/linux/centos/docker-ce.repo
    dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
    systemctl enable --now docker
    usermod -aG docker #{LOGIN}
END_OF_STRING

puts colored("9. Installiere borg und borgmatic ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Install borgmatic
    dnf install -y borgbackup python3-pip python3-setuptools
    pip3 install --upgrade pip
    pip3 install borgmatic
END_OF_STRING

puts colored("10. Setze Zeitzone ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    # Set timezone
    timedatectl set-timezone Europe/Berlin
END_OF_STRING

puts colored("Fertig, starte nun den Server neu mit: reboot now", color: :green, bold: true)
