require './include/helper.rb'
require 'base64'
require 'cgi'
require 'digest'
require 'mail'
require 'neo4j_bolt'
require './credentials.rb'
require 'faye/websocket'
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

    def self.tag_for_sid(sid)
        srand(Digest::SHA2.hexdigest(LOGIN_CODE_SALT + sid).to_i)
        (0...10).map { |x| rand(10).to_s }.join('')
    end

    def tag_for_sid(sid)
        Main.tag_for_sid(sid)
    end

    def self.tag_for_email(email)
        return Digest::SHA1.hexdigest(email)[0, 16]
    end

    def tag_for_email(email)
        Main.tag_for_email(email)
    end


    def self.refresh_nginx_config
        tag_for_email = {}
        email_for_tag = {}
        sessions_for_user = {}
        results = $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (s:Session)-[:FOR]->(u:User)
            RETURN s.sid, u.email, s;
        END_OF_QUERY
            sid = row['s.sid']
            tag = tag_for_sid(sid)
            email = row['u.email']
            sessions_for_user[email] ||= {}
            sessions_for_user[email][tag] = sid
            unless tag_for_email.include?(email)
                email_tag = tag_for_email(email)
                tag_for_email[email] = email_tag
                email_for_tag[email_tag] = email
            end
        end

        running_servers = {}
        inspect = JSON.parse(`docker network inspect workspace`)
        inspect.first['Containers'].values.each do |container|
            name = container['Name']
            next unless name[0, 8] == 'hs_code_'
            user_tag = name.sub('hs_code_', '')
            ip = container['IPv4Address'].split('/').first
            if email_for_tag[user_tag]
                running_servers[user_tag] = {
                    :ip => ip,
                    :email => email_for_tag[user_tag],
                }
            end
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
            running_servers.each_pair do |email_tag, info|
                sessions_for_user[email_for_tag[email_tag]].each_pair do |session_tag, sid|
                    f.puts <<~END_OF_STRING
                    location /#{session_tag}/ {
                        if ($cookie_sid != "#{sid}") {
                            return 403;
                        }
                        rewrite ^/#{session_tag}(.*)$ $1 break;
                        proxy_set_header Host $http_host;
                        proxy_set_header Upgrade $http_upgrade;
                        proxy_set_header Connection upgrade;
                        proxy_set_header Accept-Encoding gzip;
                        proxy_pass http://#{info[:ip]}:8443;
                    }
                    END_OF_STRING
                end
            end
        f.puts nginx_config_second_part
        end
        system("docker kill -s HUP workspace_nginx_1")
    end

    configure do
        CONSTRAINTS_LIST = [
            'User/email',
            'LoginRequest/tag',
            'Session/sid',
        ]
        INDEX_LIST = []

        setup = SetupDatabase.new()
        setup.wait_for_neo4j()
        @@clients = {}
        @@email_for_client_id = {}
        @@client_ids_for_email = {}
        $neo4j.setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
        self.refresh_nginx_config()
    end

    before '*' do
        @session_user = nil
        @sid_tag = nil
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
                                }
                                @sid_tag = tag_for_sid(first_sid)
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

    def jury_logged_in?
        return false
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

        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email})
            MERGE (u:User {email: $email})
            RETURN u.email;
        END_OF_QUERY

        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :tag => tag, :code => random_code})
            MATCH (u:User {email: $email})
            CREATE (r:LoginRequest)-[:FOR]->(u)
            SET r.tag = $tag
            SET r.code = $code
            RETURN u.email;
        END_OF_QUERY

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
        result[:du] = `du -d 0 /user/#{tag}`.split(/\s/).first.to_i
        inspect = JSON.parse(`docker inspect hs_code_#{tag}`)
        unless inspect.empty?
            result[:running] = true
            result[:ip] = inspect.first['NetworkSettings']['Networks']['workspace']['IPAddress']
        end
        result
    end

    def send_server_state
        return unless @session_user
        email = @session_user[:email]
        container_name = tag_for_email(email)
        state = get_server_state(container_name)
        (@@client_ids_for_email[email] || []).each do |client_id|
            ws = @@clients[client_id]
            ws.send({:action => :update_state, :state => state}.to_json)
        end
    end

    post '/api/start_server' do
        assert(user_logged_in?)

        email = @session_user[:email]
        container_name = tag_for_email(email)

        system("mkdir -p /user/#{container_name}/config")
        system("mkdir -p /user/#{container_name}/config/data")
        system("mkdir -p /user/#{container_name}/config/extensions")
        system("mkdir -p /user/#{container_name}/config")
        system("mkdir -p /user/#{container_name}/workspace")
        system("chown -R 1000:1000 /user/#{container_name}")
        network_name = "workspace"
        system("docker run -d -e PUID=1000 -e GUID=1000 -e TZ=Europe/Berlin -e DEFAULT_WORKSPACE=/workspace -v #{PATH_TO_HOST_DATA}/user/#{container_name}/config:/config -v #{PATH_TO_HOST_DATA}/user/#{container_name}/workspace:/workspace --network #{network_name} --name hs_code_#{container_name} hs_code_server")

        Main.refresh_nginx_config()
        send_server_state()

        respond(:yay => 'sure')
    end

    post '/api/stop_server' do
        assert(user_logged_in?)

        email = @session_user[:email]
        container_name = tag_for_email(email)

        system("docker kill hs_code_#{container_name}")
        system("docker rm hs_code_#{container_name}")

        Main.refresh_nginx_config()
        send_server_state()

        respond(:yay => 'sure')
    end

    get '/ws' do
        if Faye::WebSocket.websocket?(request.env)
            ws = Faye::WebSocket.new(request.env)

            ws.on(:open) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                ws.send({:hello => 'world'})
                @@clients[client_id] = ws
                if @session_user
                    @@email_for_client_id[client_id] = @session_user[:email]
                    @@client_ids_for_email[@session_user[:email]] ||= Set.new()
                    @@client_ids_for_email[@session_user[:email]] << client_id
                    send_server_state()
                end
                debug "Got #{@@clients.size} connected clients!"
            end

            ws.on(:close) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                @@clients.delete(client_id) if @@clients.include?(client_id)
                @@email_for_client_id.delete(client_id) if @@email_for_client_id.include?(client_id)
                if @session_user
                    @@client_ids_for_email[@session_user[:email]].delete(client_id) if @@client_ids_for_email[@session_user[:email]].include?(client_id)
                    @@client_ids_for_email.delete(@session_user[:email]) if @@client_ids_for_email.include?(@session_user[:email]) && @@client_ids_for_email[@session_user[:email]].empty?
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
                sid = RandomTag::generate(24)
                neo4j_query_expect_one(<<~END_OF_QUERY, {:sid => sid, :email => email, :expires => (DateTime.now() + 365).to_s})
                    MATCH (u:User {email: $email})
                    WITH u
                    CREATE (s:Session {sid: $sid, expires: $expires})-[:FOR]->(u)
                    RETURN s.sid AS sid;
                END_OF_QUERY
                response.set_cookie('sid',
                    :value => sid,
                    :expires => Time.new + 3600 * 24 * 365,
                    :path => '/',
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
