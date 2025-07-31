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
puts "frischen Server aus, nachdem du Schritt 02 ausgeführt und neu gestartet"
puts "hast. Wir richten nun das externe Volume ein."
puts
puts "Dieses Volume wird nun platt gemacht: #{STORAGE_DEVICE}"
puts
exit unless confirm("Bist du sicher, dass du fortfahren möchtest (j/n)?")

puts "Los geht's!"

puts colored(" Hackschule Workspace Servereinrichtung ", color: :white, bg: :blue, bold: true)

puts colored("12. Erstelle physisches Volume: #{STORAGE_DEVICE} ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    pvcreate #{STORAGE_DEVICE}
    vgcreate vdo-vg #{STORAGE_DEVICE}
END_OF_STRING

PHYS_GB = `vgs --noheadings --units g -o vg_free vdo-vg`.strip.to_f
PHYS_GB_SAFE= PHYS_GB * 0.999
VIRT_GB= PHYS_GB_SAFE * 3

puts colored("13. Erstelle logisches Volume mit #{VIRT_GB.round} GB", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    lvcreate --type vdo --name vdo1 --size #{PHYS_GB_SAFE}G --virtualsize #{VIRT_GB}G vdo-vg
END_OF_STRING

puts colored("14. Formatiere Volume mit XFS: #{STORAGE_DEVICE} ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    mkfs.xfs /dev/vdo-vg/vdo1
END_OF_STRING

puts colored("15. Erstelle /mnt/hackschule ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    mkdir /mnt/hackschule
END_OF_STRING

puts colored("16. Trage /mnt/hackschule in /etc/fstab ein ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    echo "/dev/mapper/vdo--vg-vdo1 /mnt/hackschule xfs defaults,noauto 0 0" >> /etc/fstab
    systemctl daemon-reload
END_OF_STRING

puts colored("17. Hänge /mnt/hackschule ein ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    mount /mnt/hackschule
END_OF_STRING

puts colored("18. Generiere Begrüßungsnachricht nach Login ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/banner.txt -O /home/#{LOGIN}/.banner.txt
    wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/stats.rb -O /home/#{LOGIN}/.stats.rb
    wget -q https://raw.githubusercontent.com/specht/workspace/refs/heads/master/bootstrap/git-completion.bash -O /home/#{LOGIN}/.git-completion.bash
    chmod +x /home/#{LOGIN}/.stats.rb
    chown #{LOGIN}:#{LOGIN} /home/#{LOGIN}/{.banner.txt,.stats.rb,.git-completion.bash}
END_OF_STRING

bashrc = File.read("/home/#{LOGIN}/.bashrc")
bashrc += <<~EOS
if [ -f ~/.git-completion.bash ]; then
  source ~/.git-completion.bash
fi

if [[ $- == *i* ]]; then
  echo "--------------------------------------------------------------------------------"
  cat .banner.txt
  echo "--------------------------------------------------------------------------------"
  ./.stats.rb
  echo "--------------------------------------------------------------------------------"
  echo " Nach dem Reboot nicht vergessen: "
  echo " # mount /mnt/hackschule"
  echo " # systemctl start docker"
  echo "--------------------------------------------------------------------------------"
fi
EOS
File.open("/home/#{LOGIN}/.bashrc", 'w') { |f| f.write bashrc }

