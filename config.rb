#!/usr/bin/env ruby

require 'fileutils'
require 'json'
require 'yaml'
require './src/ruby/credentials.rb'

PROFILE = [:static, :dynamic, :neo4j]

STAGING = File::dirname(File::expand_path(__FILE__)).include?('staging')
PROJECT_NAME = 'workspace'
DEV_NGINX_PORT = 8025
DEV_NEO4J_PORT = 8021
LOGS_PATH = DEVELOPMENT ? './logs' : "/home/#{ENV['USER']}/logs/#{PROJECT_NAME}"
DATA_PATH = DEVELOPMENT ? './data' : "/mnt/hackschule/#{PROJECT_NAME}"
MYSQL_DATA_PATH = File.join(DATA_PATH, 'mysql')
NEO4J_USER_DATA_PATH = File.join(DATA_PATH, 'neo4j_user')
USER_PATH = File.join(DATA_PATH, 'user')
INTERNAL_PATH = File.join(DATA_PATH, 'internal')
INVITATIONS_PATH = File.join(DATA_PATH, 'invitations')
WEB_CACHE_PATH = File.join(DATA_PATH, 'cache')
DOWNLOAD_PATH = File.join(DATA_PATH, 'dl')
NGINX_PATH = File.join(DATA_PATH, 'nginx')
NEO4J_LOGS_PATH = File::join(LOGS_PATH, 'neo4j')
NEO4J_DATA_PATH = File::join(DATA_PATH, 'neo4j')

docker_compose = {
    :services => {},
}

FileUtils::mkpath(NGINX_PATH)
FileUtils::mkpath(File.join(DATA_PATH, 'nginx-snippets'))

if PROFILE.include?(:static)
    docker_compose[:services][:nginx] = {
        :build => './docker/nginx',
        :volumes => [
            './src/static:/usr/share/nginx/html:ro',
            "#{WEB_CACHE_PATH}:/webcache:ro",
            "#{DATA_PATH}/brand:/brand:ro",
            "#{DOWNLOAD_PATH}:/dl:ro",
            "#{LOGS_PATH}:/var/log/nginx",
            "#{DATA_PATH}/nginx:/etc/nginx/conf.d",
            "#{DATA_PATH}/nginx-snippets:/etc/nginx/snippets"
        ]
    }
    docker_compose[:services][:nginx][:environment] ||= []
    # docker_compose[:services][:nginx][:environment] << "VIRTUAL_HOST=#{WEBSITE_HOST},code.#{WEBSITE_HOST},watch.#{WEBSITE_HOST}"
    if !DEVELOPMENT
        # docker_compose[:services][:nginx][:environment] << "LETSENCRYPT_HOST=#{WEBSITE_HOST},code.#{WEBSITE_HOST},watch.#{WEBSITE_HOST}"
        # docker_compose[:services][:nginx][:environment] << "LETSENCRYPT_EMAIL=#{ADMIN_USERS.first}"
        docker_compose[:services][:nginx][:labels] = []
        docker_compose[:services][:nginx][:labels] << "traefik.enable=true"
        docker_compose[:services][:nginx][:labels] << "traefik.docker.network=proxy"
        docker_compose[:services][:nginx][:labels] << "traefik.http.routers.workspace.rule=Host(`#{WEBSITE_HOST}`) || HostRegexp(`^(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)\\.#{WEBSITE_HOST.gsub('.', '\\.')}$`)"
        docker_compose[:services][:nginx][:labels] << "traefik.http.routers.workspace.entrypoints=websecure"
        docker_compose[:services][:nginx][:labels] << "traefik.http.routers.workspace.tls.certresolver=le"
        docker_compose[:services][:nginx][:labels] << "traefik.http.routers.workspace.tls.domains[0].main=#{WEBSITE_HOST}"
        docker_compose[:services][:nginx][:labels] << "traefik.http.routers.workspace.tls.domains[0].sans=*.#{WEBSITE_HOST}"
        docker_compose[:services][:nginx][:labels] << "traefik.http.services.workspace.loadbalancer.server.port=80"
    end
    if PROFILE.include?(:dynamic)
        docker_compose[:services][:nginx][:depends_on] = [
            :ruby,
            :phpmyadmin,
            :neo4japp,
        ]
    end
end

if PROFILE.include?(:dynamic)
    env = []
    env << 'DEVELOPMENT=1' if DEVELOPMENT
    env << 'STAGING=1' if STAGING
    docker_compose[:services][:ruby] = {
        :build => './docker/ruby',
        :volumes => ['./src:/src:ro',
                     "#{WEB_CACHE_PATH}:/webcache",
                     "#{USER_PATH}:/user",
                     "#{INTERNAL_PATH}:/internal",
                     "#{DATA_PATH}/brand:/brand:ro",
                     "#{INVITATIONS_PATH}:/invitations:ro",
                     "#{DATA_PATH}/tic80:/tic80",
                     "/var/run/docker.sock:/var/run/docker.sock",
                     "#{NGINX_PATH}:/nginx",
                     "#{DATA_PATH}/nginx-snippets:/nginx-snippets",
                     "#{DOWNLOAD_PATH}:/dl",
                    ],
        :environment => env,
        :working_dir => '/src/ruby',
        :privileged => true,
        # rackup's development middleware includes Rack::Lint, which rejects
        # Faye's async hijack response before Puma can take over the socket.
        :entrypoint =>  DEVELOPMENT ?
            'rerun -b --dir /src/ruby -s SIGKILL -- rackup --server puma --env production --host 0.0.0.0' :
            'rackup --server puma --env production --host 0.0.0.0'
    }
    if PROFILE.include?(:neo4j)
        docker_compose[:services][:ruby][:depends_on] ||= []
        docker_compose[:services][:ruby][:depends_on] << :neo4japp
    end
end

if PROFILE.include?(:neo4j)
    docker_compose[:services][:neo4japp] = {
        :build => './docker/neo4j',
        :volumes => ["#{NEO4J_DATA_PATH}:/data",
                     "#{NEO4J_LOGS_PATH}:/logs"]
    }
    docker_compose[:services][:neo4japp][:environment] = [
        'NEO4J_AUTH=none',
        # 'NEO4J_dbms_db_timezone=SYSTEM',
        # 'NEO4J_dbms_allow__upgrade=true',
        # 'NEO4J_metrics=false',
    ]
    docker_compose[:services][:neo4japp][:user] = '1000'
end

docker_compose[:services][:mysql] = {
    :image => 'mysql:8.4.11',
    :command => [
        "--require_secure_transport=OFF",
        "--mysqlx=0"
        # "--mysql-native-password=ON"
    ],
    :volumes => ["#{MYSQL_DATA_PATH}:/var/lib/mysql"],
    :user => '1000',
    :restart => 'always',
    :environment => {
        'MYSQL_ROOT_HOST' => '%',
        'MYSQL_ROOT_PASSWORD' => MYSQL_ROOT_PASSWORD
    },
}

docker_compose[:services][:neo4j] = {
    :image => 'neo4j:2026.06.0-enterprise',
    # :command => ["--default-authentication-plugin=mysql_native_password"],
    :volumes => ["#{NEO4J_USER_DATA_PATH}:/data"],
    :user => '1000',
    :restart => 'always',
    :environment => {
        'NEO4J_ACCEPT_LICENSE_AGREEMENT' => 'yes',
        'NEO4J_AUTH' => "neo4j/#{NEO4J_ROOT_PASSWORD}",
        'NEO4J_EDITION' => 'enterprise',
        'NEO4J_dbms_security_auth__enabled' => 'true',
    },
}

docker_compose[:services][:phpmyadmin] = {
    :image => 'phpmyadmin/phpmyadmin:5.2.3',
    :restart => 'always',
    :depends_on => [:mysql],
    :environment => {
        'PMA_ABSOLUTE_URI' => PHPMYADMIN_WEB_ROOT,
        'PMA_HOST' => 'mysql',
        'UPLOAD_LIMIT' => '128M',
    },
}

if DEVELOPMENT
    docker_compose[:services][:nginx][:ports] = ["0.0.0.0:#{DEV_NGINX_PORT}:80"]
    if PROFILE.include?(:neo4j)
        docker_compose[:services][:neo4japp][:ports] ||= []
        docker_compose[:services][:neo4japp][:ports] << "127.0.0.1:#{DEV_NEO4J_PORT}:7474"
    end
end

docker_compose[:services].values.each do |x|
    x[:networks] = ['internal']
end
docker_compose[:networks] = {
    :internal => {
        :driver => 'bridge'
    },
    :user => {
        :driver => 'bridge',
        :enable_ipv6 => false,
    }
}
[:nginx, :ruby, :mysql, :neo4j].each do |service_name|
    service = docker_compose[:services][service_name]
    service[:networks] << 'user'
    service[:labels] ||= []
    service[:labels] << 'hackschule.workspace.peer_firewall.infrastructure=true'
end

docker_compose[:services][:peer_firewall] = {
    :build => './docker/peer-firewall',
    :network_mode => 'host',
    :cap_drop => ['ALL'],
    :cap_add => ['NET_ADMIN'],
    :security_opt => ['no-new-privileges:true'],
    :read_only => true,
    :volumes => ['/var/run/docker.sock:/var/run/docker.sock:ro'],
    :environment => {
        'WORKSPACE_NETWORK' => "#{PROJECT_NAME}_user",
        'INFRASTRUCTURE_LABEL' => 'hackschule.workspace.peer_firewall.infrastructure',
        'PEER_TCP_PORTS' => '1234,40000-40999',
        'PEER_UDP_PORTS' => '1234,40000-40999',
    },
    :restart => 'always',
    :healthcheck => {
        :test => ['CMD-SHELL', 'nft list table bridge hackschule_workspace >/dev/null 2>&1'],
        :interval => '5s',
        :timeout => '2s',
        :retries => 5,
        :start_period => '5s',
    },
}

unless DEVELOPMENT
    docker_compose[:services].values.each do |x|
        x[:restart] = :always
    end
    docker_compose[:networks][:proxy] = {
        :external => true
    }
    docker_compose[:services][:nginx][:networks] << 'proxy'
end

File::open('docker-compose.yaml', 'w') do |f|
    f.puts "# NOTICE: don't edit this file directly, use config.rb instead!\n"
    f.write(JSON::parse(docker_compose.to_json).to_yaml)
end

FileUtils::mkpath(LOGS_PATH)
FileUtils::mkpath(File.join(LOGS_PATH, 'neo4j'))
if PROFILE.include?(:dynamic)
    FileUtils::cp(['src/ruby/Gemfile', 'src/ruby/Gemfile.lock'], 'docker/ruby/')
end
if PROFILE.include?(:neo4j)
    FileUtils::mkpath(NEO4J_DATA_PATH)
end
FileUtils::mkpath(USER_PATH)
FileUtils::mkpath(INTERNAL_PATH)
FileUtils::mkpath(INVITATIONS_PATH)
template_path = File.join(INVITATIONS_PATH, '_template.txt')
File.open(template_path, 'w') do |f|
    f.puts <<~EOS
        # Der Workspace findet alle Dateien, die auf .txt enden und nicht
        # _template.txt heißen – dort stehen alle Einladungen drin.
        #
        # Du kannst mit »>« Gruppen mit Mitgliedern definieren:
        > Lehrkräfte
        Scott Clarke <scott@example.com>

        # Du kannst mit »+« definieren, wer in welcher Gruppe als Lehrer
        # Zugriff auf Daten der SuS hat:
        > Klasse 5a
        + scott@example.com
        Max Mustermann <max@example.com>
    EOS
end
FileUtils::mkpath(WEB_CACHE_PATH)
FileUtils::mkpath(File.join(DATA_PATH, 'tic80'))
FileUtils::mkpath(MYSQL_DATA_PATH)
FileUtils::mkpath(NEO4J_USER_DATA_PATH)
FileUtils::mkpath(File.join(DATA_PATH, 'internal'))
FileUtils::mkpath(File.join(DATA_PATH, 'brand'))
FileUtils::mkpath(File.join(DATA_PATH, 'mysql'))
FileUtils::mkpath(File.join(DATA_PATH, 'dl'))

`docker compose 2> /dev/null`
DOCKER_COMPOSE = ($? == 0) ? 'docker compose' : 'docker-compose'
system("#{DOCKER_COMPOSE} --compatibility --project-name #{PROJECT_NAME} #{ARGV.map { |x| '"' + x + '"'}.join(' ')}")
