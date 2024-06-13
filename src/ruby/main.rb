require './include/helper.rb'
require 'base64'
require 'cgi'
require 'digest'
require 'mail'
require 'neo4j_bolt'
require 'nokogiri'
require './credentials.rb'
require 'faye/websocket'
require 'redcarpet'
require 'rouge'
require 'securerandom'
require 'sinatra/base'
require 'sinatra/cookies'

Neo4jBolt.bolt_host = 'neo4j'
Neo4jBolt.bolt_port = 7687

Faye::WebSocket.load_adapter('thin')

CACHE_BUSTER = SecureRandom.alphanumeric(12)

class Neo4jGlobal
    include Neo4jBolt
end

$neo4j = Neo4jGlobal.new

def assert(condition, message = 'assertion failed')
    raise message unless condition
end

def debug(message, index = 0)
    index = 0
    begin
        while index < caller_locations.size - 1 && ['transaction', 'neo4j_query', 'neo4j_query_expect_one'].include?(caller_locations[index].base_label)
            index += 1
        end
    rescue
        index = 0
    end
    # STDERR.puts caller_locations.to_yaml
    l = caller_locations[index]
    ls = ''
    begin
        ls = "#{l.path.sub('/app/', '')}:#{l.lineno} @ #{l.base_label}"
    rescue
        ls = "#{l[0].sub('/app/', '')}:#{l[1]}"
    end
    STDERR.puts "#{DateTime.now.strftime('%H:%M:%S')} [#{ls}] #{message}"
end

class RandomTag
    BASE_31_ALPHABET = '0123456789bcdfghjklmnpqrstvwxyz'
    def self.to_base31(i)
        result = ''
        while i > 0
            result += BASE_31_ALPHABET[i % 31]
            i /= 31
        end
        result
    end

    def self.generate(length = 12)
        self.to_base31(SecureRandom.hex(length).to_i(16))[0, length]
    end
end

def mail_html_to_plain_text(s)
    s.gsub('<p>', "\n\n").gsub(/<br\s*\/?>/, "\n").gsub(/<\/?[^>]*>/, '').strip
end

def deliver_mail(plain_text = nil, &block)
    mail = Mail.new do
        charset = 'UTF-8'
        message = self.instance_eval(&block)
        if plain_text.nil?
            html_part do
                content_type 'text/html; charset=UTF-8'
                body message
            end

            text_part do
                content_type 'text/plain; charset=UTF-8'
                body mail_html_to_plain_text(message)
            end
        else
            text_part do
                content_type 'text/plain; charset=UTF-8'
                body plain_text
            end
        end
    end
    if DEVELOPMENT
        STDERR.puts "Not sending mail in development mode!"
        STDERR.puts '-' * 40
        STDERR.puts "From:    #{mail.from.join('; ')}"
        STDERR.puts "To:      #{mail.to.join('; ')}"
        STDERR.puts "Subject: #{mail.subject}"
        STDERR.puts mail.text_part
        STDERR.puts '-' * 40
    else
        mail.deliver!
    end
end

class SetupDatabase
    include Neo4jBolt

    def setup(main)
        delay = 1
        10.times do
            begin
                neo4j_query("MATCH (n) RETURN n LIMIT 1;")
                setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
                debug "Setup finished."
                break
            rescue
                debug $!
                debug "Retrying setup after #{delay} seconds..."
                sleep delay
                delay += 1
            end
        end
    end
end

class Main < Sinatra::Base
    include Neo4jBolt
    helpers Sinatra::Cookies

    def self.fs_tag_for_email(email)
        Digest::SHA2.hexdigest(email).to_i(16).to_s(36)[0, 16]
    end

    def fs_tag_for_email(email)
        Main.fs_tag_for_email(email)
    end

    def self.tag_for_sid(sid)
        Digest::SHA2.hexdigest(LOGIN_CODE_SALT + sid)[0, 16].to_i(16).to_s(36)[0, 8].downcase
    end

    def tag_for_sid(sid)
        Main.tag_for_sid(sid)
    end

    def self.server_sid_for_email(email)
        return Digest::SHA2.hexdigest(LOGIN_CODE_SALT + email)
    end

    def server_sid_for_email(email)
        Main.server_sid_for_email(email)
    end

    def self.refresh_nginx_config
        running_servers = {}
        inspect = JSON.parse(`docker network inspect workspace`)
        inspect.first['Containers'].values.each do |container|
            name = container['Name']
            next unless name[0, 8] == 'hs_code_'
            fs_tag = name.sub('hs_code_', '')
            ip = container['IPv4Address'].split('/').first
            running_servers[fs_tag] = {
                :ip => ip,
            }
        end

        nginx_config_first_part = <<~END_OF_STRING
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

                location /cache {
                    rewrite ^/cache(.*)$ $1 break;
                    root /webcache;
                    include /etc/nginx/mime.types;
                }

                location @ruby {
                    proxy_pass http://workspace_ruby_1:9292;
                    proxy_set_header Host $host;
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection Upgrade;
                }
        END_OF_STRING

        nginx_config_second_part = <<~END_OF_STRING
            }
        END_OF_STRING

        File.open('/nginx/default.conf', 'w') do |f|
            f.puts nginx_config_first_part
            $neo4j.neo4j_query("MATCH (u:User) RETURN u;").each do |row|
                user = row['u']
                fs_tag = fs_tag_for_email(user[:email])
                if running_servers.include?(fs_tag)
                    f.puts <<~END_OF_STRING
                    location /#{user[:server_tag]}/ {
                        if ($cookie_server_sid != "#{user[:server_sid]}") {
                            return 403;
                        }
                        rewrite ^/#{user[:server_tag]}(.*)$ $1 break;
                        proxy_set_header Host $http_host;
                        proxy_set_header Upgrade $http_upgrade;
                        proxy_set_header Connection upgrade;
                        proxy_set_header Accept-Encoding gzip;
                        proxy_pass http://#{running_servers[fs_tag][:ip]}:8443;
                    }
                    END_OF_STRING
                    if user[:share_tag] && user[:share_tag] =~ /^[a-z0-9]{48}$/
                        f.puts <<~END_OF_STRING
                            location /#{user[:share_tag]}/ {
                                rewrite ^/#{user[:share_tag]}(.*)$ $1 break;
                                proxy_set_header Host $http_host;
                                proxy_set_header Upgrade $http_upgrade;
                                proxy_set_header Connection upgrade;
                                proxy_set_header Accept-Encoding gzip;
                                proxy_pass http://#{running_servers[fs_tag][:ip]}:8443;
                            }
                        END_OF_STRING
                    end
                    $neo4j.neo4j_query(<<~END_OF_QUERY, {:email => user[:email]}).each do |row|
                        MATCH (:User)-[r:WATCHING]->(u:User {email: $email})
                        RETURN r.watch_tag;
                    END_OF_QUERY
                        watch_tag = row['r.watch_tag']
                        f.puts <<~END_OF_STRING
                            location /#{watch_tag}/ {
                                rewrite ^/#{watch_tag}(.*)$ $1 break;
                                proxy_set_header Host $http_host;
                                proxy_set_header Upgrade $http_upgrade;
                                proxy_set_header Connection upgrade;
                                proxy_set_header Accept-Encoding gzip;
                                proxy_pass http://#{running_servers[fs_tag][:ip]}:8443;
                            }
                        END_OF_STRING
                    end
                end
            end
            f.puts nginx_config_second_part
        end
        system("docker kill -s HUP workspace_nginx_1")
    end

    def self.convert_image(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            STDERR.puts "Copying #{image_path} to cache..."
            FileUtils.cp(image_path, target_path)
        end
        target_path_512 = "/webcache/#{image_sha1}-512.webp"
        unless FileUtils.uptodate?(target_path_512, [target_path])
            system("convert #{target_path} -resize 512x #{target_path_512}")
        end
        image_sha1
    end

    def self.parse_content
        hyphenation_map = {}
        File.read('/src/content/hyphenation.txt').split("\n").each do |line|
            line.strip!
            hyphenation_map[line.gsub('-', '')] = line.gsub('-', '&shy;')
        end
        sections = YAML.load(File.read('/src/content/sections.yaml'))
        @@section_order = sections.map { |section| section['key'] }
        @@sections = {}
        sections.each do |section|
            @@sections[section['key']] = {}
            section.each_pair do |k, v|
                @@sections[section['key']][k.to_sym] = v
            end
            @@sections[section['key']][:entries] = []
        end
        @@content = {}
        StringIO.open do |io|
            redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true})
            Dir['/src/content/*/*.md'].each do |path|
                markdown = File.read(path)
                hyphenation_map.each_pair do |a, b|
                    markdown.gsub!(a, b)
                end
                slug = File.basename(path, '.md')
                html = redcarpet.render(markdown)
                root = Nokogiri::HTML(html)
                meta = root.css('.meta').first
                if meta
                    meta = YAML.load(meta)
                    if meta.include?('visible')
                        if meta['visible'] == false
                            next
                        elsif meta['visible'] == 'development'
                            next unless DEVELOPMENT
                        end
                    end
                end

                root.css('img').each do |img|
                    src = img.attr('src')
                    image_path = File.join(File.dirname(path), src)
                    next unless File.exist?(image_path)
                    image_sha1 = convert_image(image_path)
                    img['src'] = "/cache/#{image_sha1}.webp"
                    if img.classes.include?('full')
                        img.wrap("<div class='scroll-x'>")
                    end
                end
                root.css('pre').each do |pre|
                    code = pre.css('code').first
                    next if code.nil?
                    language = code.attr('class')
                    formatter = Rouge::Formatters::HTML.new
                    lexer = nil
                    case language
                    when 'java'
                        lexer = Rouge::Lexers::Java.new
                    when 'python'
                        lexer = Rouge::Lexers::Python.new
                    when 'ruby'
                        lexer = Rouge::Lexers::Ruby.new
                    when 'bash'
                        lexer = Rouge::Lexers::Shell.new
                    when 'javascript'
                        lexer = Rouge::Lexers::Javascript.new
                    end
                    next if lexer.nil?
                    pre.content = ''
                    pre << formatter.format(lexer.lex(code.text))
                end
                html = root.to_html
                meta = root.css('.meta').first
                @@content[slug] = {
                    :html => html
                }
                if meta
                    meta = YAML.load(meta)
                    section = meta['section']
                    @@sections[section][:entries] << slug
                    if meta['image']
                        parts = meta['image'].split(':')
                        sha1 = convert_image(File.join(File.dirname(path), parts[0]))
                        @@content[slug][:image] = "/cache/#{sha1}.webp"
                        @@content[slug][:image_x] = (parts[1] || '50').to_i
                        @@content[slug][:image_y] = (parts[2] || '50').to_i
                    end
                end
                begin
                    @@content[slug][:title] = root.css('h1').first.text
                rescue
                end
                begin
                    @@content[slug][:abstract] = root.css('.abstract').first.text
                rescue
                end
                begin
                    @@content[slug][:image] ||= root.css('img').first.attr('src')
                    @@content[slug][:image_x] ||= 50
                    @@content[slug][:image_y] ||= 50
                rescue
                end
            end
        end
    end

    configure do
        CONSTRAINTS_LIST = [
            'User/email',
            'User/server_tag',
            'User/share_tag',
            'LoginRequest/tag',
            'Session/sid',
        ]
        INDEX_LIST = []

        setup = SetupDatabase.new()
        setup.wait_for_neo4j()
        @@clients = {}
        @@email_for_client_id = {}
        @@client_ids_for_email = {}
        @@threads_for_client_id = {}
        $neo4j.setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
        self.refresh_nginx_config()
        self.parse_content()
    end

    before '*' do
        @session_user = nil
        if request.cookies.include?('sid')
            sid = request.cookies['sid']
            if (sid.is_a? String) && (sid =~ /^[0-9A-Za-z]+$/)
                first_sid = sid.split(',').first
                if first_sid =~ /^[0-9A-Za-z]+$/
                    results = neo4j_query(<<~END_OF_QUERY, :sid => first_sid).to_a
                        MATCH (s:Session {sid: $sid})-[:FOR]->(u:User)
                        RETURN s, u;
                    END_OF_QUERY
                    if results.size == 1
                        begin
                            session = results.first['s']
                            session_expiry = session[:expires]
                            if DateTime.parse(session_expiry) > DateTime.now
                                email = results.first['u'][:email]
                                @session_user = {
                                    :email => email.downcase,
                                    :server_tag => results.first['u'][:server_tag],
                                    :share_tag => results.first['u'][:share_tag],
                                }
                                # set server_sid cookie if it's not set or out of date
                                expires = Time.new + 3600 * 24 * 365
                                [:server_sid].each do |key|
                                    if request.cookies[key.to_s] != results.first['u'][key]
                                        response.set_cookie(key.to_s,
                                        :value => results.first['u'][key],
                                        :expires => expires,
                                        :path => '/',
                                        :httponly => true,
                                        :secure => DEVELOPMENT ? false : true)
                                    end
                                end
                            end
                        rescue
                            # something went wrong, delete the session
                            results = neo4j_query(<<~END_OF_QUERY, :sid => first_sid).to_a
                                MATCH (s:Session {sid: $sid})
                                DETACH DELETE s;
                            END_OF_QUERY
                        end
                    end
                end
            end
        end
    end

    def this_is_a_page_for_logged_in_users!
        if @session_user.nil?
            redirect "#{WEB_ROOT}/login", 302
        end
    end

    def this_is_a_page_for_logged_in_admins!
        assert(admins_logged_in?)
    end

    def user_logged_in?
        return (!@session_user.nil?)
    end

    def admin_logged_in?
        return user_logged_in? && ADMIN_USERS.include?(@session_user[:email])
    end

    def gen_server_tag()
        RandomTag::generate(12)
    end

    def gen_server_sid()
        RandomTag::generate(48)
    end

    def gen_share_tag()
        RandomTag::generate(48)
    end

    post '/api/request_login' do
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].downcase.strip

        found = false
        File.open('invitations.txt') do |f|
            f.each_line do |line|
                line.strip!
                next if line.empty?
                if line[0] == '@'
                    parts = email.split('@')
                    if parts.size == 2 && "@#{parts.last}" == line
                        found = true
                        break
                    end
                else
                    if line == email
                        found = true
                        break
                    end
                end
            end
        end

        raise 'no invitation found' unless found

        tag = RandomTag::generate(12)
        srand(Digest::SHA2.hexdigest(LOGIN_CODE_SALT).to_i + (Time.now.to_f * 1000000).to_i)
        random_code = (0..5).map { |x| rand(10).to_s }.join('')
        random_code = '123456' if DEVELOPMENT

        user = neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email})['u']
            MERGE (u:User {email: $email})
            RETURN u;
        END_OF_QUERY

        if user[:server_tag].nil? || user[:server_sid].nil?
            neo4j_query(<<~END_OF_QUERY, {:email => email, :server_tag => gen_server_tag(), :server_sid => gen_server_sid()})
                MATCH (u:User {email: $email})
                SET u.server_tag = COALESCE(u.server_tag, $server_tag)
                SET u.server_sid = COALESCE(u.server_sid, $server_sid)
            END_OF_QUERY
        end

        # remove all stale login requests
        ts = Time.now.to_i - 60 * 10
        neo4j_query(<<~END_OF_QUERY, {:ts => ts})
            MATCH (l:LoginRequest)
            WHERE COALESCE(l.ts_expiry, 0) < $ts
            DETACH DELETE l;
        END_OF_QUERY
        # remove all pending login requests for this user
        neo4j_query(<<~END_OF_QUERY, {:email => email})
            MATCH (r:LoginRequest)-[:FOR]->(u:User {email: $email})
            DETACH DELETE r;
        END_OF_QUERY
        # add new login requests for this user
        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :tag => tag, :code => random_code, :now => Time.now.to_i})
            MATCH (u:User {email: $email})
            CREATE (r:LoginRequest)-[:FOR]->(u)
            SET r.tag = $tag
            SET r.code = $code
            SET r.ts_expiry = $now
            RETURN u.email;
        END_OF_QUERY
        broadcast_login_codes()

        deliver_mail do
            to data[:email]
            # bcc SMTP_FROM
            from SMTP_FROM

            subject "Dein Anmeldecode lautet #{random_code}"

            StringIO.open do |io|
                io.puts "<p>Hallo!</p>"
                io.puts "<p>Dein Anmeldecode lautet:</p>"
                io.puts "<p style='font-size: 200%;'>#{random_code}</p>"
                io.puts "<p>Der Code ist für zehn Minuten gültig. Nachdem du dich angemeldet hast, bleibst du für ein ganzes Jahr angemeldet (falls du dich nicht wieder abmeldest).</p>"
                io.puts "<p>Falls du diese E-Mail nicht angefordert hast, hat jemand versucht, sich mit deiner E-Mail-Adresse auf <a href='https://#{WEBSITE_HOST}/'>https://#{WEBSITE_HOST}/</a> anzumelden. In diesem Fall musst du nichts weiter tun (es sei denn, du befürchtest, dass jemand anderes Zugriff auf dein E-Mail-Konto hat – dann solltest du dein E-Mail-Passwort ändern).</p>"
                io.puts "<p>Viele Grüße,<br />Michael Specht</p>"
                io.string
            end
        end
        respond(:ok => 'yay', :tag => tag)
    end

    post '/api/impersonate' do
        assert(admin_logged_in?)
        data = parse_request_data(:required_keys => [:email])
        email = data[:email]
        sid = request.cookies['sid']
        neo4j_query(<<~END_OF_STRING, {:sid => sid, :email => email})
            MATCH (s:Session {sid: $sid})-[r:FOR]->(:User), (u:User {email: $email})
            DELETE r
            CREATE (s)-[:FOR]->(u);
        END_OF_STRING
        respond(:yay => 'sure')
    end

    def get_server_state(tag)
        result = {}
        result[:tag] = tag
        result[:running] = false
        inspect = JSON.parse(`docker inspect hs_code_#{tag}`)
        unless inspect.empty?
            result[:running] = true
            result[:ip] = inspect.first['NetworkSettings']['Networks']['workspace']['IPAddress']
        end
        result
    end

    def start_server(email)
        container_name = fs_tag_for_email(email)

        state = get_server_state(container_name)
        return if state[:running]

        system("mkdir -p /user/#{container_name}/config")
        system("mkdir -p /user/#{container_name}/workspace")
        config_path = "/user/#{container_name}/workspace/.local/share/code-server/User/settings.json"
        unless File.exist?(config_path)
            FileUtils.mkpath(File.dirname(config_path))
            File.open(config_path, 'w') do |f|
                config = {}
                f.write config.to_json
            end
        end
        config = JSON.parse(File.read(config_path))
        config ||= {}
        config['files.exclude'] ||= {}
        config['files.exclude']['**/.*'] = true
        File.open(config_path, 'w') do |f|
            f.write config.to_json
        end
        system("chown -R 1000:1000 /user/#{container_name}")
        network_name = "workspace"
        system("docker run --cpus=2 -d --rm -e PUID=1000 -e GUID=1000 -e TZ=Europe/Berlin -e DEFAULT_WORKSPACE=/workspace -v #{PATH_TO_HOST_DATA}/user/#{container_name}/config:/config -v #{PATH_TO_HOST_DATA}/user/#{container_name}/workspace:/workspace --network #{network_name} --name hs_code_#{container_name} hs_code_server")

        Main.refresh_nginx_config()
    end

    def stop_server(email)
        container_name = fs_tag_for_email(email)

        system("docker kill hs_code_#{container_name}")

        Main.refresh_nginx_config()
    end

    post '/api/start_server' do
        assert(user_logged_in?)

        email = @session_user[:email]
        start_server(email)

        respond(:yay => 'sure', :server_tag => @session_user[:server_tag])
    end

    post '/api/start_server_with_share_tag' do
        data = parse_request_data(:required_keys => [:share_tag])
        share_tag = data[:share_tag]

        user = neo4j_query_expect_one(<<~END_OF_QUERY, :share_tag => share_tag)['u']
            MATCH (u:User {share_tag: $share_tag})
            RETURN u;
        END_OF_QUERY

        start_server(user[:email])

        respond(:yay => 'sure', :share_tag => user[:share_tag])
    end

    post '/api/start_server_as_admin' do
        assert(admin_logged_in?)
        data = parse_request_data(:required_keys => [:email])
        email = data[:email]

        # purge all WATCHING relationships (should be zero or one)
        neo4j_query(<<~END_OF_QUERY, {:email_self => @session_user[:email]})
            MATCH (u_self:User {email: $email_self})-[r:WATCHING]->(u:User)
            DELETE r;
        END_OF_QUERY

        # create new WATCHING relationship
        watch_tag = RandomTag.generate(48)
        neo4j_query_expect_one(<<~END_OF_QUERY, {:email_self => @session_user[:email], :email_other => email, :watch_tag => watch_tag})
            MATCH (u_self:User {email: $email_self})
            MATCH (u_other:User {email: $email_other})
            CREATE (u_self)-[r:WATCHING]->(u_other)
            SET r.watch_tag = $watch_tag
            RETURN r;
        END_OF_QUERY

        start_server(email)
        Main.refresh_nginx_config()

        respond(:yay => 'sure', :watch_tag => watch_tag)
    end

    post '/api/reset_server' do
        assert(user_logged_in?)

        email = @session_user[:email]
        stop_server(email)
        container_name = fs_tag_for_email(email)
        system("rm -rf /user/#{container_name}")

        # assign new server_tag and server_sid
        server_tag = gen_server_tag()
        server_sid = gen_server_sid()
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :server_tag => server_tag, :server_sid => server_sid})
            MATCH (u:User {email: $email})
            SET u.server_tag = $server_tag
            SET u.server_sid = $server_sid
        END_OF_STRING

        respond(:yay => 'sure')
    end

    post '/api/share_server' do
        assert(user_logged_in?)

        # create server_tag for user
        share_tag = gen_share_tag()
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :share_tag => share_tag})
            MATCH (u:User {email: $email})
            SET u.share_tag = $share_tag;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure', :share_tag => share_tag)
    end

    post '/api/unshare_server' do
        assert(user_logged_in?)

        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]})
            MATCH (u:User {email: $email})
            REMOVE u.share_tag;
        END_OF_STRING

        Main.refresh_nginx_config()

        respond(:yay => 'sure')
    end

    def bytes_to_str(ai_Size)
        if ai_Size < 1024
            return "#{ai_Size} B"
        elsif ai_Size < 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0)} kB"
        elsif ai_Size < 1024 * 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0)} MB"
        elsif ai_Size < 1024 * 1024 * 1024 * 1024
            return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0)} GB"
        end
        return "#{sprintf('%1.1f', ai_Size.to_f / 1024.0 / 1024.0 / 1024.0 / 1024.0)} TB"
    end

    def print_content_overview()
        Main.parse_content if DEVELOPMENT
        StringIO.open do |io|
            @@section_order.each do |section_key|
                section = @@sections[section_key]
                next if section[:entries].empty?
                io.puts "<h2><img class='circle' src='#{section[:icon]}'> #{section[:label]}</h2>"
                io.puts "<hr>"
                io.puts "<div class='row'>"
                section[:entries].each.with_index do |slug, index|
                    content = @@content[slug]
                    io.puts "<div class='col-sm-12 col-md-6 col-lg-4'>"
                    io.puts "<a href='/#{slug}' class='tutorial_card'>"
                    io.puts "<h4>#{content[:title]}</h4>"
                    io.puts "<div class='ratio ratio-16x9 mb-2'>"
                    io.puts "<img src='#{content[:image].sub('.webp', '-512.webp')}' style='object-position: #{content[:image_x]}% #{content[:image_y]}%;'>"
                    io.puts "</div>"
                    io.puts "<p class='abstract'>#{content[:abstract]}</p>"
                    io.puts "</a>"
                    io.puts "</div>"
                    # io.puts "<hr>"
                end
                io.puts "</div>"
            end
            io.string
        end
    end

    def print_workspaces()
        email_for_tag = {}
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User) RETURN u.email;
        END_OF_STRING
            email = row['u.email']
            email_for_tag[fs_tag_for_email(email)] = email
        end
        return '' unless admin_logged_in?
        du_for_fs_tag = {}
        begin
            du_for_fs_tag = JSON.parse(File.read('/internal/du_for_fs_tag.json'))
        rescue
        end

        info_for_tag = {}
        JSON.parse(`docker inspect workspace`).each do |entry|
            entry['Containers'].each_pair do |id, container|
                name = container['Name']
                next unless name[0, 8] == 'hs_code_'
                info_for_tag[name.sub('hs_code_', '')] = {
                    :ip => container['IPv4Address'],
                }
            end
        end

        StringIO.open do |io|
            io.puts "<div style='max-width: 100%; overflow-x: auto;'>"
            io.puts "<table class='table' id='table_admin_workspaces'>"
            io.puts "<tr>"
            io.puts "<th>Tag</th>"
            io.puts "<th>E-Mail</th>"
            io.puts "<th>IP</th>"
            io.puts "<th style='width: 5.2em;'>CPU</th>"
            io.puts "<th>RAM</th>"
            io.puts "<th>Disk Usage</th>"
            io.puts "<th>Workspace</th>"
            io.puts "</tr>"
            Dir['/user/*'].each do |path|
                next unless File.basename(path) =~ /^[0-9a-z]{16}$/
                user_tag = path.split('/').last
                io.puts "<tr id='tr_hs_code_#{user_tag}'>"
                io.puts "<td><code>#{user_tag}</code></td>"
                io.puts "<td>#{email_for_tag[user_tag]}</td>"
                io.puts "<td class='td_ip'>#{(info_for_tag[user_tag] || {})[:ip] || '&ndash;'}</td>"
                io.puts "<td class='td_cpu'></td>"
                io.puts "<td class='td_ram'></td>"
                if du_for_fs_tag[user_tag]
                    io.puts "<td>#{bytes_to_str(du_for_fs_tag[user_tag] * 1024)}</td>"
                else
                    io.puts "<td>&ndash;</td>"
                end
                io.puts "<td><button class='btn btn-sm btn-success bu-open-workspace-as-admin' data-email='#{email_for_tag[user_tag]}'><i class='fa fa-code'></i>&nbsp;Workspace öffnen</button></td>"
                io.puts "</tr>"
            end
            io.puts "</table>"
            io.puts "</div>"
            io.string
        end
    end

    def broadcast_login_codes
        return if @@clients.empty?
        lines = []
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (l:LoginRequest)-[:FOR]->(u:User)
            RETURN l.code, u.email
            ORDER BY l.expiry;
        END_OF_STRING
            lines << { :email => row['u.email'], :code => row['l.code'] }
        end
        @@clients.each_pair do |client_id, ws|
            ws.send({:action => 'login_codes', :lines => lines}.to_json)
        end
    end

    get '/ws' do
        assert(admin_logged_in?)
        if Faye::WebSocket.websocket?(request.env)
            ws = Faye::WebSocket.new(request.env)

            ws.on(:open) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                ws.send({:hello => 'world'})
                @@clients[client_id] = ws
                @@email_for_client_id[client_id] = @session_user[:email]
                @@client_ids_for_email[@session_user[:email]] ||= []
                @@client_ids_for_email[@session_user[:email]] << client_id
                @@threads_for_client_id[client_id] ||= Thread.new do
                    command = "docker stats --format \"{{ json . }}\""
                    lines = {}
                    IO.popen(command).each_line do |line|
                        if line[0, 1].ord == 0x1b
                            ws.send({:stats => lines}.to_json)
                            lines = {}
                        end
                        line = line[line.index('{'), line.size]
                        stat_line = JSON.parse(line)
                        name = stat_line['Name']
                        if name[0, 8] == 'hs_code_'
                            lines[name.sub('hs_code_', '')] = stat_line
                        end
                    end
                end
                broadcast_login_codes()
                debug "Got #{@@clients.size} connected clients!"
            end

            ws.on(:close) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                @@clients.delete(client_id) if @@clients.include?(client_id)
                @@email_for_client_id.delete(client_id) if @@email_for_client_id.include?(client_id)
                @@client_ids_for_email[@session_user[:email]].delete(client_id) if @@client_ids_for_email[@session_user[:email]].include?(client_id)
                @@client_ids_for_email.delete(@session_user[:email]) if @@client_ids_for_email[@session_user[:email]].empty?
                if @@threads_for_client_id.include?(client_id)
                    @@threads_for_client_id[client_id].kill
                    @@threads_for_client_id.delete(client_id)
                end
                debug "Got #{@@clients.size} connected clients!"
            end

            ws.on(:message) do |msg|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                begin
                    request = {}
                    unless msg.data.empty?
                        request = JSON.parse(msg.data)
                    end
                    if request['hello'] == 'world'
                        ws.send({:status => 'welcome'}.to_json)
                    end
                rescue StandardError => e
                    STDERR.puts e
                end
            end
            ws.rack_response
        end
    end

    get '/*' do
        path = request.path
        assert(!path.include?('..'))
        slug = nil
        Main.parse_content() if DEVELOPMENT
        if path[0, 7] == '/share/'
            share_tag = path.sub('/share/', '')
            STDERR.puts "OPENING SHARE WITH SHARE TAG #{share_tag}"
            share_user = neo4j_query_expect_one(<<~END_OF_STRING, :share_tag => share_tag)['u']
                MATCH (u:User {share_tag: $share_tag})
                RETURN u;
            END_OF_STRING
            path = '/share.html'
        end
        if @@content.include?(path[1, path.size - 1])
            slug = path[1, path.size - 1]
            path = '/a.html'
        end
        if path == '/'
            path = '/index.html'
        end
        confirm_tag = nil
        if path[0, 3] == '/l/'
            rest = path[3, path.size - 3].split('/')
            path = '/index.html'
            tag = rest[0]
            code = rest[1]
            begin
                email = neo4j_query_expect_one(<<~END_OF_QUERY, {:tag => tag, :code => code})['email']
                    MATCH (r:LoginRequest {tag: $tag, code: $code})-[:FOR]->(u:User)
                    RETURN u.email AS email;
                END_OF_QUERY
                neo4j_query(<<~END_OF_QUERY, {:tag => tag, :code => code})
                    MATCH (r:LoginRequest {tag: $tag, code: $code})-[:FOR]->(u:User)
                    DETACH DELETE r;
                END_OF_QUERY
                broadcast_login_codes()
                sid = RandomTag::generate(24)
                neo4j_query_expect_one(<<~END_OF_QUERY, {:sid => sid, :email => email, :expires => (DateTime.now() + 365).to_s})
                    MATCH (u:User {email: $email})
                    WITH u
                    CREATE (s:Session {sid: $sid, expires: $expires})-[:FOR]->(u)
                    RETURN s.sid AS sid;
                END_OF_QUERY
                expires = Time.new + 3600 * 24 * 365
                response.set_cookie('sid',
                    :value => sid,
                    :expires => expires,
                    :path => '/',
                    :httponly => true,
                    :secure => DEVELOPMENT ? false : true)
                response.set_cookie('server_sid',
                    :value => server_sid_for_email(email),
                    :expires => expires,
                    :path => "/#{fs_tag_for_email(email)}",
                    :httponly => true,
                    :secure => DEVELOPMENT ? false : true)
            end
            redirect "#{WEB_ROOT}/", 302
        end
        if path[0, 7] == '/logout'
            path = '/index.html'
            neo4j_query(<<~END_OF_QUERY, {:sid => request.cookies['sid']})
                MATCH (s:Session {sid: $sid})
                DETACH DELETE s;
            END_OF_QUERY
            response.set_cookie('sid',
                :value => nil,
                :expires => Time.new + 3600 * 24 * 365,
                :path => '/',
                :httponly => true,
                :secure => DEVELOPMENT ? false : true)
            redirect "#{WEB_ROOT}/", 302
        end
        path = path + '.html' unless path.include?('.')
        respond_with_file(File.join(@@static_dir, path)) do |content, mime_type|
            if mime_type == 'text/html'
                template = File.read(File.join(@@static_dir, '_template.html'))
                template.sub!('#{CONTENT}', content)
                s = template
                while true
                    index = s.index('#{')
                    break if index.nil?
                    length = 2
                    balance = 1
                    while index + length < s.size && balance > 0
                        c = s[index + length]
                        balance -= 1 if c == '}'
                        balance += 1 if c == '{'
                        length += 1
                    end
                    code = s[index + 2, length - 3]
                    begin
                        s[index, length] = eval(code).to_s || ''
                    rescue
                        STDERR.puts "Error while evaluating:"
                        STDERR.puts code
                        raise
                    end
                end
                s
            end
        end
    end
end
