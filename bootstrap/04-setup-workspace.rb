#!/usr/bin/env ruby

require 'etc'
require 'yaml'
require 'io/console'
require './common.rb'

CHARS = [('A'..'Z'), ('a'..'z'), ('0'..'9')].map(&:to_a).flatten

def gen_password()
    Array.new(24) { CHARS.sample(random: Random.new) }.join
end

config = YAML.load(File.read('config.yaml'))
LOGIN = config['login']
DOMAIN = config['domain']
EMAIL = config['email']

unless Process.uid = 0
    puts "Dieses Skript muss als root laufen."
    exit(1)
end

if LOGIN.nil? || DOMAIN.nil? || EMAIL.nil?
    puts "Bevor es losgehen kann, musst du in der config.yaml ein paar Angaben machen:"
    puts
    puts "login          : Dein Login auf dem Server (nach außen nicht sichtbar)"
    puts "domain         : Die Domain, unter der der Workspace gehostet werden soll"
    puts "email          : Deine E-Mail-Adresse (gleichzeitig Admin-E-Mail-Adresse)"
    puts
    exit(1)
end

_ = `mountpoint /mnt/hackschule`
if $? != 0
    puts "Achtung, /mnt/hackschule ist nicht eingehängt."
    puts "Bitte hänge das Volume aus, bevor wir anfangen:"
    puts
    puts "mount /mnt/hackschule"
    puts
    exit(1)
end

_ = `docker ps`
if $? != 0
    puts "Achtung, Docker läuft noch nicht."
    puts "Bitte starte Docker, bevor wir anfangen:"
    puts
    puts "systemctl start docker"
    puts
    exit(1)
end

puts colored(" ACHTUNG ", color: :white, bg: :red)
puts
puts "Dieses Skript nimmt umfangreiche Änderungen an diesem Server vor und"
puts "richtet den Hackschule Workspace ein. Führe dieses Skript nur auf einem"
puts "frischen Server aus, nachdem du Schritt 03 ausgeführt hast."
puts
exit unless confirm("Bist du sicher, dass du fortfahren möchtest (j/n)?")

puts "Los geht's!"

puts colored(" Hackschule Workspace Servereinrichtung ", color: :white, bg: :blue, bold: true)

puts colored("20. Erstelle Verzeichnis: /mnt/hackschule/workspace ", color: :cyan, bold: true)
system("mkdir -p /mnt/hackschule/workspace && chown #{LOGIN}:#{LOGIN} /mnt/hackschule/workspace")

user_info = Etc.getpwnam(LOGIN)
Process::GID.change_privilege(user_info.gid)
Process::UID.change_privilege(user_info.uid)
ENV['USER'] = user_info.name
ENV['HOME'] = user_info.dir
ENV['LOGNAME'] = user_info.name
ENV['SHELL'] = user_info.shell

puts colored("21. Klone Workspace-Repository ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
  cd
  git clone https://github.com/specht/workspace.git
END_OF_STRING

puts colored("22. Schreibe /home/#{LOGIN}/workspace/src/ruby/credentials.rb ", color: :cyan, bold: true)
config = <<~EOS
    DEVELOPMENT = false

    PATH_TO_HOST_DATA = "/mnt/hackschule/workspace"

    WEBSITE_HOST = "#{DOMAIN}"
    WEB_ROOT = "https://#{DOMAIN}"
    PHPMYADMIN_WEB_ROOT = "https://#{DOMAIN}/phpmyadmin/"
    PGADMIN_WEB_ROOT = "https://#{DOMAIN}/pgadmin/"
    NEO4J_WEB_ROOT = "https://#{DOMAIN}/neo4j/browser/"

    LOGIN_CODE_SALT = '#{gen_password()}'

    # Diese Werte bitte für den E-Mail-Versand anpassen:
    SMTP_SERVER = 'smtps.udag.de'
    IMAP_SERVER = 'imaps.udag.de'
    SMTP_USER = 'info@example.com'
    SMTP_PASSWORD = 'smtp_password'
    SMTP_DOMAIN = 'smtp_domain'
    SMTP_FROM = 'Absender <E-Mail-Adresse>'

    ADMIN_USERS = ['#{EMAIL}']

    MYSQL_ROOT_PASSWORD = '#{gen_password()}'
    MYSQL_PASSWORD_SALT = '#{gen_password()}'
    POSTGRES_ROOT_PASSWORD = '#{gen_password()}'
    POSTGRES_PASSWORD_SALT = '#{gen_password()}'
    PGADMIN_PASSWORD = '#{gen_password()}'
    NEO4J_ROOT_PASSWORD = '#{gen_password()}'
    NEO4J_PASSWORD_SALT = '#{gen_password()}'

    if defined? Mail
        Mail.defaults do
        delivery_method :smtp, {
            :address => SMTP_SERVER,
            :port => 587,
            :domain => SMTP_DOMAIN,
            :user_name => SMTP_USER,
            :password => SMTP_PASSWORD,
            :authentication => 'login',
            :enable_starttls_auto => true
        }
        end
    end
EOS

Dir.chdir("/home/#{LOGIN}/workspace")

File.open('src/ruby/credentials.rb', 'w') { |f| f.puts config }

puts colored("23. Baue Image für Workspace ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    ./config.rb build
END_OF_STRING

puts colored("24. Baue Image für VS Code ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    ./build-image.sh
END_OF_STRING

puts colored("25. Baue TIC-80 ", color: :cyan, bold: true)
run_with_scrolling_tail(<<~END_OF_STRING)
    ./build-tic80.sh
END_OF_STRING
