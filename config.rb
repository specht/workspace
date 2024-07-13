#!/usr/bin/env ruby

require 'fileutils'
require 'json'
require 'yaml'
require './src/ruby/credentials.rb'

PROFILE = [:static, :dynamic, :neo4j]

STAGING = File::dirname(File::expand_path(__FILE__)).include?('staging')
warn_level = $VERBOSE
$VERBOSE = nil
DEVELOPMENT = !(ENV['QTS_DEVELOPMENT'].nil?)
$VERBOSE = warn_level
PROJECT_NAME = 'workspace'
DEV_NGINX_PORT = 8025
DEV_NEO4J_PORT = 8021
LOGS_PATH = DEVELOPMENT ? './logs' : "/home/micha/logs/#{PROJECT_NAME}"
DATA_PATH = DEVELOPMENT ? './data' : "/mnt/hackschule/#{PROJECT_NAME}"
# MYSQL_DATA_PATH = File.join(DATA_PATH, 'mysql')
# POSTGRES_DATA_PATH = File.join(DATA_PATH, 'postgres')
USER_PATH = File.join(DATA_PATH, 'user')
INTERNAL_PATH = File.join(DATA_PATH, 'internal')
WEB_CACHE_PATH = File.join(DATA_PATH, 'cache')
NGINX_PATH = File.join(DATA_PATH, 'nginx')
NEO4J_LOGS_PATH = File::join(LOGS_PATH, 'neo4j')
NEO4J_DATA_PATH = File::join(DATA_PATH, 'neo4j')

docker_compose = {
    :services => {},
}

FileUtils::mkpath(NGINX_PATH)

if PROFILE.include?(:static)
    docker_compose[:services][:nginx] = {
        :build => './docker/nginx',
        :volumes => [
            './src/static:/usr/share/nginx/html:ro',
            "#{WEB_CACHE_PATH}:/webcache:ro",
            "#{LOGS_PATH}:/var/log/nginx",
            "#{DATA_PATH}/nginx:/etc/nginx/conf.d"
        ]
    }
    if !DEVELOPMENT
        docker_compose[:services][:nginx][:environment] = [
            'VIRTUAL_HOST=workspace.hackschule.de',
            'LETSENCRYPT_HOST=workspace.hackschule.de',
            'LETSENCRYPT_EMAIL=specht@gymnasiumsteglitz.de'
        ]
        docker_compose[:services][:nginx][:expose] = ['80']
    end
    docker_compose[:services][:nginx][:links] = ["ruby:#{PROJECT_NAME}_ruby_1"]
    nginx_config = <<~END_OF_STRING
        log_format custom '$http_x_forwarded_for - $remote_user [$time_local] "$request" '
                          '$status $body_bytes_sent "$http_referer" '
                          '"$http_user_agent" "$request_time"';

        map $sent_http_content_type $expires {
            default                         off;
            text/html                       epoch;
            text/css                        max;
            application/javascript          max;
            ~image/                         max;
            ~font/                          max;
            application/x-font-ttf          max;
            application/x-font-otf          max;
            application/font-woff           max;
            application/font-woff2          max;
        }

        server {
            listen 80;
            server_name localhost;
            client_max_body_size 100M;
            expires $expires;

            gzip on;
            gzip_comp_level 6;
            gzip_min_length 256;
            gzip_buffers 16 8k;
            gzip_proxied any;
            gzip_types
                text/plain
                text/css
                text/js
                text/xml
                text/javascript
                application/javascript
                application/x-javascript
                application/json
                application/xml
                application/rss+xml
                image/svg+xml;

            access_log /var/log/nginx/access.log custom;

            charset utf-8;

            location / {
                root /usr/share/nginx/html;
                include /etc/nginx/mime.types;
                try_files $uri @ruby;
            }

            location @ruby {
                proxy_pass http://#{PROJECT_NAME}_ruby_1:9292;
                proxy_set_header Host $host;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection Upgrade;
            }
        }
    END_OF_STRING
    File::open(File.join(NGINX_PATH, 'default.conf'), 'w') do |f|
        f.write nginx_config
    end
    if PROFILE.include?(:dynamic)
        docker_compose[:services][:nginx][:depends_on] = [:ruby]
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
                     "#{DATA_PATH}/tic80:/tic80",
                     "/var/run/docker.sock:/var/run/docker.sock",
                     "#{NGINX_PATH}:/nginx",
                    ],
        :environment => env,
        :working_dir => '/src/ruby',
        :privileged => true,
        :entrypoint =>  DEVELOPMENT ?
            'rerun -b --dir /src/ruby -s SIGKILL \'rackup --host 0.0.0.0\'' :
            'rackup --host 0.0.0.0'
    }
    if PROFILE.include?(:neo4j)
        docker_compose[:services][:ruby][:depends_on] ||= []
        docker_compose[:services][:ruby][:depends_on] << :neo4j
        docker_compose[:services][:ruby][:links] = ['neo4j:neo4j']
    end
end

if PROFILE.include?(:neo4j)
    docker_compose[:services][:neo4j] = {
        :build => './docker/neo4j',
        :volumes => ["#{NEO4J_DATA_PATH}:/data",
                     "#{NEO4J_LOGS_PATH}:/logs"]
    }
    docker_compose[:services][:neo4j][:environment] = [
        'NEO4J_AUTH=none',
        'NEO4J_dbms_logs__timezone=SYSTEM',
        'NEO4J_dbms_allow__upgrade=true',
    ]
    docker_compose[:services][:neo4j][:user] = '1000'
end

# docker_compose[:services][:mysql] = {
#     :image => 'mysql/mysql-server',
#     :command => ["--default-authentication-plugin=mysql_native_password"],
#     :volumes => ["#{MYSQL_DATA_PATH}:/var/lib/mysql"],
#     :restart => 'always',
#     :environment => {
#         'MYSQL_ROOT_HOST' => '%',
#         'MYSQL_ROOT_PASSWORD' => MYSQL_ROOT_PASSWORD
#     },
#     :ports => ['0.0.0.0:3306:3306'],
# }

# docker_compose[:services][:postgres] = {
#     :image => 'postgres',
#     # :command => ["--default-authentication-plugin=mysql_native_password"],
#     :volumes => ["#{POSTGRES_DATA_PATH}:/var/lib/postgres"],
#     :restart => 'always',
#     :environment => {
#         'POSTGRES_PASSWORD' => POSTGRES_ROOT_PASSWORD
#     },
#     :ports => ['0.0.0.0:5432:5432'],
# }

docker_compose[:services][:tensorflowjs] = {
    :image => 'evenchange4/docker-tfjs-converter',
    :volumes => ["#{INTERNAL_PATH}:/internal"],
    :restart => 'always',
    :tty => true,
}

docker_compose[:services].values.each do |x|
    # x[:networks] = [:workspace]
    x[:network_mode] = 'default'
end

docker_compose[:networks] = {
    :workspace => {
        :name => 'workspace',
    }
}

if DEVELOPMENT
    docker_compose[:services][:nginx][:ports] = ["0.0.0.0:#{DEV_NGINX_PORT}:80"]
    if PROFILE.include?(:neo4j)
        docker_compose[:services][:neo4j][:ports] ||= []
        docker_compose[:services][:neo4j][:ports] << "127.0.0.1:#{DEV_NEO4J_PORT}:7474"
        docker_compose[:services][:neo4j][:ports] << "127.0.0.1:7687:7687"
    end
end

unless DEVELOPMENT
    docker_compose[:services].values.each do |x|
        x[:restart] = :always
    end
end

File::open('docker-compose.yaml', 'w') do |f|
    f.puts "# NOTICE: don't edit this file directly, use config.rb instead!\n"
    f.write(JSON::parse(docker_compose.to_json).to_yaml)
end

FileUtils::mkpath(LOGS_PATH)
if PROFILE.include?(:dynamic)
    FileUtils::cp('src/ruby/Gemfile', 'docker/ruby/')
end
if PROFILE.include?(:neo4j)
    FileUtils::mkpath(NEO4J_DATA_PATH)
end
FileUtils::mkpath(USER_PATH)
FileUtils::mkpath(INTERNAL_PATH)
FileUtils::mkpath(WEB_CACHE_PATH)
FileUtils::mkpath(File.join(DATA_PATH, 'tic80'))
# FileUtils::mkpath(MYSQL_DATA_PATH)
# FileUtils::mkpath(POSTGRES_DATA_PATH)

`docker compose 2> /dev/null`
DOCKER_COMPOSE = ($? == 0) ? 'docker compose' : 'docker-compose'
system("#{DOCKER_COMPOSE} --compatibility --project-name #{PROJECT_NAME} #{ARGV.map { |x| '"' + x + '"'}.join(' ')}")
