#!/usr/bin/env ruby
require 'yaml'

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

def confirm(message)
    print("#{message}")
    loop do
        c = getc()
        puts c
    end
end

puts colored(" Hackschule Workspace Servereinrichtung ", color: :cyan, bg: :blue, bold: true)

config = YAML.load(File.read('config.yaml'))

LOGIN = config['login']
PUBLIC_KEY = config['public_key']
DOMAIN = config['domain']

unless  Process.uid == 0
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

exit unless confirm("Bist du sicher?")