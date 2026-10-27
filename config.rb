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
TUTORIAL_SCREENSHOT = {
    :email => 'student@example.com',
    :login_code => '123456',
    :workspace_user => 'student',
    :width => 1853 / 1.8,
    :height => 929 / 1.8,
    # The Workspace tab: 44 CSS pixels taller. Since VS Code 1.140 the panel
    # is a card with margins and shows two terminal rows fewer; this gives
    # them back. Previews keep :height.
    :workspace_height => 1008 / 1.8,
    :zoom => 1.0,
    :capture_scale => 1.8,
    :desktop_scale_factor => 1.0,
}.freeze
LOGS_PATH = DEVELOPMENT ? './logs' : "/home/#{ENV['USER']}/logs/#{PROJECT_NAME}"
DATA_PATH = DEVELOPMENT ? './data' : "/mnt/hackschule/#{PROJECT_NAME}"
TUTORIAL_SCREENSHOT_GIT_CACHE_PATH = File.join(DATA_PATH, 'tutorial-screenshot-git-cache')
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
    if DEVELOPMENT
        env << "TUTORIAL_SCREENSHOT_EMAIL=#{TUTORIAL_SCREENSHOT[:email]}"
        env << "TUTORIAL_SCREENSHOT_WORKSPACE_USER=#{TUTORIAL_SCREENSHOT[:workspace_user]}"
    end
    docker_compose[:services][:ruby] = {
        :build => './docker/ruby',
        :volumes => ['./src:/src:ro',
                     "#{WEB_CACHE_PATH}:/webcache",
                     "#{USER_PATH}:/user",
                     "#{INTERNAL_PATH}:/internal",
                     "#{DATA_PATH}/brand:/brand:ro",
                     "#{INVITATIONS_PATH}:/invitations:ro",
                     "#{DATA_PATH}/tic80:/tic80",
                     # Not Docker itself: the guard's socket (docker_guard below).
                     "#{DATA_PATH}/docker-guard:/run/docker-guard",
                     "#{NGINX_PATH}:/nginx",
                     "#{DATA_PATH}/nginx-snippets:/nginx-snippets",
                     "#{DOWNLOAD_PATH}:/dl",
                     # The running commit, for /admin »Betrieb«.
                     './.git:/git:ro',
                    ],
        :environment => env,
        :working_dir => '/src/ruby',
        #:privileged => true,
        # rackup's development middleware includes Rack::Lint, which rejects
        # Faye's async hijack response before Puma can take over the socket.
        # Up to 12 threads (Puma's default is 5): a download of »Meine Daten«
        # keeps one for its whole length (at most 2 at a time, my_data.rb).
        :entrypoint =>  DEVELOPMENT ?
            'rerun -b --dir /src/ruby -s SIGKILL -- rackup --server puma --env production --host 0.0.0.0 -O Threads=0:12' :
            'rackup --server puma --env production --host 0.0.0.0 -O Threads=0:12'
    }
    if PROFILE.include?(:neo4j)
        docker_compose[:services][:ruby][:depends_on] ||= []
        docker_compose[:services][:ruby][:depends_on] << :neo4japp
    end
end

if DEVELOPMENT && PROFILE.include?(:dynamic) && PROFILE.include?(:static)
    # WebGL in the screenshots (the anaglyph scenes, the Backrooms, VS Code's
    # terminal) renders on an NVIDIA graphics card when the host has one and
    # Docker knows the nvidia runtime (nvidia-container-toolkit), several
    # times faster than in software (SwiftShader), which is used otherwise.
    # TUTORIAL_SCREENSHOT_GPU=0 ./config.rb up -d forces software.
    tutorial_screenshot_gpu = ENV['TUTORIAL_SCREENSHOT_GPU'] != '0' &&
        system('nvidia-smi -L > /dev/null 2>&1') &&
        `docker info --format '{{json .Runtimes}}' 2> /dev/null`.include?('"nvidia"')
    docker_compose[:services][:tutorial_screenshots] = {
        :build => {
            :context => './src/tutorial-screenshots',
            :dockerfile => '../../docker/tutorial-screenshots/Dockerfile',
        },
        :volumes => [
            './src/content:/content',
            "#{USER_PATH}:/user",
            "#{TUTORIAL_SCREENSHOT_GIT_CACHE_PATH}:/git-cache",
        ],
        :environment => {
            'TUTORIAL_SCREENSHOT_BASE_URL' => WEB_ROOT,
            'TUTORIAL_SCREENSHOT_EMAIL' => TUTORIAL_SCREENSHOT[:email],
            'TUTORIAL_SCREENSHOT_LOGIN_CODE' => TUTORIAL_SCREENSHOT[:login_code],
            'TUTORIAL_SCREENSHOT_WORKSPACE_USER' => TUTORIAL_SCREENSHOT[:workspace_user],
            'TUTORIAL_SCREENSHOT_WIDTH' => TUTORIAL_SCREENSHOT[:width].to_s,
            'TUTORIAL_SCREENSHOT_HEIGHT' => TUTORIAL_SCREENSHOT[:height].to_s,
            'TUTORIAL_SCREENSHOT_WORKSPACE_HEIGHT' => TUTORIAL_SCREENSHOT[:workspace_height].to_s,
            'TUTORIAL_SCREENSHOT_ZOOM' => TUTORIAL_SCREENSHOT[:zoom].to_s,
            'TUTORIAL_SCREENSHOT_CAPTURE_SCALE' => TUTORIAL_SCREENSHOT[:capture_scale].to_s,
            'TUTORIAL_SCREENSHOT_DESKTOP_SCALE_FACTOR' => TUTORIAL_SCREENSHOT[:desktop_scale_factor].to_s,
            'TUTORIAL_SCREENSHOT_GPU' => tutorial_screenshot_gpu ? 'nvidia' : 'none',
        },
        :extra_hosts => [
            'host.docker.internal:host-gateway',
        ],
        :shm_size => '1gb',
    }
    if tutorial_screenshot_gpu
        docker_compose[:services][:tutorial_screenshots][:runtime] = 'nvidia'
        docker_compose[:services][:tutorial_screenshots][:environment]['NVIDIA_VISIBLE_DEVICES'] = 'all'
        # graphics: the card's Vulkan libraries, not only CUDA
        docker_compose[:services][:tutorial_screenshots][:environment]['NVIDIA_DRIVER_CAPABILITIES'] = 'all'
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
        # Disable the default CSV metrics exporter (one growing file per metric/database).
        'NEO4J_server_metrics_csv_enabled' => 'false',
        'NEO4J_dbms_security_auth__enabled' => 'true',
        # One database per person, each small: its transaction log not
        # reserved in advance (256 MB each by default, 9.8 GB for 38 people)
        # and started anew every 16 MB, so that old logs can go.
        'NEO4J_db_tx__log_preallocate' => 'false',
        'NEO4J_db_tx__log_rotation_size' => '16MiB',
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
# The application's own Neo4j (accounts, sessions, groups) has no password:
# it shares a network with the backend only, not with nginx or the students'
# databases on `internal`, which face student traffic.
if docker_compose[:services][:neo4japp]
    # (not internal: the development port of the Neo4j browser needs a gateway)
    docker_compose[:networks][:app] = { :driver => 'bridge' }
    docker_compose[:services][:neo4japp][:networks] = ['app']
    docker_compose[:services][:ruby][:networks] << 'app'
end

# The case server of »Ein Krimi im Terminal« (docs/entwicklung.md, »Der
# Fallserver«), only with CASE_SERVER = true in credentials.rb: museum.bitstein
# on the students' network, reachable on port 22 and no further (its own
# peer-firewall label, not infrastructure), and alarmanlage.bitstein only on
# a network of their own, reachable only from the museum.
require './src/ruby/include/case_server.rb'
if CaseServer.enabled?
    case_secret = CaseServer.secret(LOGIN_CODE_SALT)
    case_service = lambda do |role|
        {
            :build => { :context => './docker/case-server', :args => { 'ROLE' => role } },
            :hostname => role,
            :environment => { 'CASE_SERVER_SECRET' => case_secret },
            :volumes => ["#{DATA_PATH}/case-server/#{role}:/keys"],
            :tmpfs => ['/tmp:size=64m,mode=1733'],
            :mem_limit => '512m',
            :pids_limit => 400,
            :cpus => 1.0,
            :restart => 'always',
        }
    end
    docker_compose[:services][:case_museum] = case_service.call('museum').merge(
        :networks => { 'user' => { :aliases => [CaseServer::MUSEUM] }, 'case' => { :aliases => [CaseServer::MUSEUM] } },
        :labels => ['hackschule.workspace.peer_firewall.case_server=true'],
    )
    docker_compose[:services][:case_alarm] = case_service.call('alarmanlage').merge(
        :networks => { 'case' => { :aliases => [CaseServer::ALARM] } },
    )
    docker_compose[:networks][:case] = { :driver => 'bridge', :internal => true }
    %w[museum alarmanlage].each { |role| FileUtils.mkpath(File.join(DATA_PATH, 'case-server', role)) }
end

# The Git server of the groups (docs/plan-git-gruppen.md), only with
# GIT_SERVER = true in credentials.rb: git.workspace on the students' network,
# reachable on port 22 and no further, like the case server (whose
# peer-firewall label it shares). The backend writes the keys and the access
# list into data/git-server/config and creates the repositories in
# data/git-repos; the server reads the one and serves the other.
require './src/ruby/include/git_server.rb'
if GitServer.enabled?
    %w[git-repos git-server/config git-server/host].each { |dir| FileUtils.mkpath(File.join(DATA_PATH, dir)) }
    # sshd ignores authorized_keys in a folder others may write to (StrictModes).
    FileUtils.chmod(0o755, File.join(DATA_PATH, 'git-server/config'))
    docker_compose[:services][:git_server] = {
        :build => './docker/git-server',
        :hostname => 'git',
        :volumes => ["#{DATA_PATH}/git-repos:/srv/git",
                     "#{DATA_PATH}/git-server/config:/etc/git-server:ro",
                     "#{DATA_PATH}/git-server/host:/keys"],
        :networks => { 'user' => { :aliases => [GitServer::HOST] } },
        :labels => ['hackschule.workspace.peer_firewall.case_server=true'],
        :tmpfs => ['/tmp:size=64m,mode=1733'],
        :mem_limit => '512m',
        :pids_limit => 200,
        :cpus => 1.0,
        :restart => 'always',
    }
    if docker_compose[:services][:ruby]
        docker_compose[:services][:ruby][:volumes] += ["#{DATA_PATH}/git-repos:/git-repos",
                                                       "#{DATA_PATH}/git-server:/git-server"]
    end
end

# The only service with the Docker socket next to the peer firewall: it
# lets the backend start and stop Workspaces and exec into them and the
# databases, and refuses everything else (docker/docker-guard/guard.rb,
# src/ruby/include/docker_policy.rb). The backend reaches it through
# DOCKER_HOST.
FileUtils.mkpath(File.join(DATA_PATH, 'docker-guard'))
docker_compose[:services][:docker_guard] = {
    :image => 'ruby:3.4.10-alpine3.24',
    :command => ['ruby', '/guard/guard.rb'],
    :volumes => ['/var/run/docker.sock:/var/run/docker.sock',
                 './docker/docker-guard:/guard:ro',
                 './src/ruby/include/docker_policy.rb:/policy/docker_policy.rb:ro',
                 "#{DATA_PATH}/docker-guard:/run/docker-guard"],
    :environment => {
        'GUARD_IMAGES' => 'hs_code_server',
        'GUARD_BIND_PREFIX' => "#{PATH_TO_HOST_DATA}/user/",
        'GUARD_NETWORKS' => "#{PROJECT_NAME}_user,none",
        'GUARD_EXEC_CONTAINERS' => "#{PROJECT_NAME}_mysql_1,#{PROJECT_NAME}_neo4j_1",
        'GUARD_HUP_CONTAINERS' => "#{PROJECT_NAME}_nginx_1",
    },
    :network_mode => 'none',
    :restart => 'always',
}
docker_compose[:services][:ruby][:environment] << 'DOCKER_HOST=unix:///run/docker-guard/docker.sock'
(docker_compose[:services][:ruby][:depends_on] ||= []) << :docker_guard

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
        'PEER_TCP_PORTS' => '1234',
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

# What the services write to their output (e-mail addresses among it) is
# overwritten after 3 × 10 MB, as the Datenschutzerklärung says
# (src/ruby/include/privacy.rb).
docker_compose[:services].values.each do |x|
    x[:logging] = { :driver => 'json-file', :options => { 'max-size' => '10m', 'max-file' => '3' } }
end

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
FileUtils::mkpath(TUTORIAL_SCREENSHOT_GIT_CACHE_PATH) if DEVELOPMENT
if DEVELOPMENT
    File.write(
        File.join(INVITATIONS_PATH, '_tutorial_screenshots.txt'),
        "> Tutorial Screenshots\nTutorial Screenshots <#{TUTORIAL_SCREENSHOT[:email]}>\n"
    )
end
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
