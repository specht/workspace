require './include/helper.rb'
require './include/automatron.rb'
require 'base64'
require 'cgi'
require 'digest'
require 'mail'
require 'mysql2'
require 'neo4j_bolt'
require 'nokogiri'
require 'faye/websocket'
require 'open3'
require 'redcarpet'
require 'rouge'
require 'securerandom'
require 'set'
require 'sinatra/base'
require 'sinatra/cookies'

require './credentials.rb'

Neo4jBolt.bolt_host = 'neo4j'
Neo4jBolt.bolt_port = 7687

Faye::WebSocket.load_adapter('thin')

CACHE_BUSTER = SecureRandom.alphanumeric(12)

MODULE_ORDER = [:workspace, :phpmyadmin, :pgadmin, :neo4j, :tic80]
MODULE_LABELS = {
    :workspace => 'Workspace',
    :phpmyadmin => 'phpMyAdmin',
    :pgadmin => 'pgAdmin',
    :neo4j => 'Neo4j',
    :tic80 => 'TIC-80',
}

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

    helpers do
        def svg_from_dot(dot_str)
            # Use Graphviz 'dot' from PATH to turn DOT into SVG (no temp files needed).
            svg, err, status = Open3.capture3('dot', '-Tsvg', stdin_data: dot_str)
            halt 500, { error: "Graphviz failed", detail: err }.to_json unless status.success?
            svg
        end

        def safe_regex(str)
            # very lightweight guardrails for classroom use:
            # - non-empty
            # - length cap (avoid pathological inputs)
            halt 400, { error: "regex must be a non-empty string" }.to_json if str.nil? || str.strip.empty?
            halt 413, { error: "regex too long" }.to_json if str.length > 2000
            str
        end
    end

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
        STDERR.puts ">>> Refreshing nginx config..."
        running_servers = {}
        inspect = JSON.parse(`docker network inspect bridge`)
        inspect.first['Containers'].values.each do |container|
            name = container['Name']
            next unless name[0, 8] == 'hs_code_'
            fs_tag = name.sub('hs_code_', '')
            ip = container['IPv4Address'].split('/').first
            running_servers[fs_tag] = {
                :ip => ip,
            }
        end
        STDERR.puts ">>> Got running servers: #{running_servers.to_json}"

        # $neo4j.neo4j_query(<<~END_OF_QUERY, {:ts => Time.now.to_i})
        #     MATCH (u:User)
        #     WHERE u.temp_server_sid_for_pgadmin_expires <= $ts
        #     REMOVE u.temp_server_sid_for_pgadmin_expires;
        # END_OF_QUERY
        # emails_and_server_tags = $neo4j.neo4j_query(<<~END_OF_QUERY).to_a.map { |x| [x['u.email'], x['u.server_sid']] }
        #     MATCH (u:User) WHERE u.temp_server_sid_for_pgadmin_expires IS NOT NULL RETURN u.email, u.server_sid;
        # END_OF_QUERY
        emails_and_server_tags = []

        # STDERR.puts "Got #{emails_and_server_tags.size} emails and server tags: #{emails_and_server_tags.to_yaml}"

        STDERR.puts ">>> Fetching users..."
        users = []
        # Here's an ugly hack: for some reason, after a day, this results in
        # Errno::EPIPE - Broken pipe, so we catch that error, and if it failed,
        # just recreate the neo4j connection and try again
        2.times do
            begin
                users = $neo4j.neo4j_query("MATCH (u:User) OPTIONAL MATCH (u)-[:TAKES]->(t:Test {running: TRUE}) RETURN u, t;").to_a
                break
            rescue
                $neo4j = Neo4jGlobal.new
            end
        end

        STDERR.puts ">>> Creating nginx config..."

        sid_ip_pairs = []

        users.each do |row|
            user = row['u']
            test = row['t']
            email_with_test_tag = "#{user[:email]}#{(test || {})[:tag]}"
            server_tag = "#{user[:server_tag]}#{(test || {})[:tag]}"
            fs_tag = fs_tag_for_email(email_with_test_tag)
            if running_servers.include?(fs_tag)
                sid_ip_pairs << "#{user[:server_sid]} #{running_servers[fs_tag][:ip]}:8443;"
            end
        end
        STDERR.puts "sid_ip_pairs:"
        STDERR.puts sid_ip_pairs.join("\n")

        watch_tag_ip_pairs = []

        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (:User)-[r:WATCHING]->(u:User)
            RETURN r.watch_tag, u;
        END_OF_QUERY
            watch_tag = row['r.watch_tag']
            user = row['u']
            fs_tag = fs_tag_for_email(user[:email])
            if running_servers.include?(fs_tag)
                watch_tag_ip_pairs << "#{watch_tag} #{running_servers[fs_tag][:ip]}:8443;"
            end
        end

        STDERR.puts "watch_tag_ip_pairs:"
        STDERR.puts watch_tag_ip_pairs.join("\n")

        nginx_config_first_part = <<~END_OF_STRING
            log_format custom '$http_x_forwarded_for - $remote_user [$time_local] "$request" '
                            '$status $body_bytes_sent "$http_referer" '
                            '"$http_user_agent" "$request_time"';

            map_hash_bucket_size 128;

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

            # Map session cookie -> target upstream (regenerate when sessions change)
            # You can generate this map block when a workspace starts/stops.
            map $cookie_hs_server_sid $workspace_upstream {
                default "";
                #{sid_ip_pairs.join("\n")}
            }
            map $cookie_hs_watch_tag $workspace_upstream_watch {
                default "";
                #{watch_tag_ip_pairs.join("\n")}
            }

            # Proper Upgrade header for WebSockets
            map $http_upgrade $connection_upgrade {
                default upgrade;
                ''      close;
            }

            # map $cookie_hs_server_sid $pgadmin_user {
            #    default "";
            #    #{emails_and_server_tags.map { |x| "\"#{x[1]}\" \"#{x[0]}\";" }.join("\n")}
            # }

            # code.workspace.hackschule.de – route by session cookie
            server {
                listen 80;
                server_name code.#{WEBSITE_HOST};

                # gzip/expires/etc as you like…

                location / {
                    if ($workspace_upstream = "") { return 403; }

                    proxy_set_header Host $http_host;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection upgrade;
                    proxy_set_header Accept-Encoding gzip;
                    proxy_pass http://$workspace_upstream;
                }
            }

            server {
                listen 80;
                server_name watch.#{WEBSITE_HOST};

                # gzip/expires/etc as you like…

                location / {
                    if ($workspace_upstream_watch = "") { return 403; }

                    proxy_set_header Host $http_host;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection upgrade;
                    proxy_set_header Accept-Encoding gzip;
                    proxy_pass http://$workspace_upstream_watch;
                }
            }

            server {
                listen 80;
                server_name #{WEBSITE_HOST};
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

                    #{users.map { |row| user = row['u']; server_tag = user[:server_tag]; fs_tag = fs_tag_for_email(user[:email]); running_servers.include?(fs_tag) ? "if ($http_referer ~* \"/#{server_tag}/proxy/([0-9]{4})/\") { set $port $1; set $new_url_#{server_tag} \"/#{server_tag}/proxy/$port$request_uri\"; } if ($new_url_#{server_tag}) { return 301 $new_url_#{server_tag}; }\n\n" : ''}.join("")}

                    try_files $uri @ruby;
                }

                location /cache {
                    rewrite ^/cache(.*)$ $1 break;
                    root /webcache;
                    include /etc/nginx/mime.types;
                }

                location /brand {
                    rewrite ^/brand(.*)$ $1 break;
                    root /brand;
                    include /etc/nginx/mime.types;
                }

                location /dl {
                    rewrite ^/dl(.*)$ $1 break;
                    root /dl;
                    include /etc/nginx/mime.types;
                }

                location /phpmyadmin {
                    rewrite ^/phpmyadmin(.*)$ $1 break;
                    try_files $uri @phpmyadmin;
                }

                location /pgadmin {
                    # proxy_set_header Remote-User $pgadmin_user;
                    proxy_set_header X-Real-IP $remote_addr;
                    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                    proxy_set_header X-Forwarded-Proto $scheme;
                    proxy_pass http://pgadmin_1:80;
                    proxy_set_header Host $host;
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection Upgrade;
                }

                location /neo4j {
                    rewrite ^/neo4j(.*)$ $1 break;
                    try_files $uri @neo4j_browser;
                }

                location /bolt {
                    rewrite ^/bolt(.*)$ $1 break;
                    try_files $uri @neo4j_bolt;
                }

                location @ruby {
                    proxy_pass http://ruby_1:9292;
                    proxy_set_header Host $host;
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection Upgrade;
                }

                location @phpmyadmin {
                    proxy_pass http://phpmyadmin_1:80;
                    proxy_set_header Host $host;
                    proxy_http_version 1.1;
                    proxy_set_header Upgrade $http_upgrade;
                    proxy_set_header Connection Upgrade;
                }

                # location @neo4j_browser {
                #     proxy_pass http://neo4j_user_1:7474;
                #     proxy_set_header Host $host;
                #     proxy_http_version 1.1;
                #     proxy_set_header Upgrade $http_upgrade;
                #     proxy_set_header Connection Upgrade;
                # }

                # location @neo4j_bolt {
                #     proxy_pass http://neo4j_user_1:7687;
                #     proxy_set_header Host $host;
                #     proxy_http_version 1.1;
                #     proxy_set_header Upgrade $http_upgrade;
                #     proxy_set_header Connection Upgrade;
                # }
        END_OF_STRING

        STDERR.puts ">>> Writing nginx config..."

        File.open('/nginx/default.conf', 'w') do |f|
            f.puts nginx_config_first_part
            users.each do |row|
                user = row['u']
                test = row['t']
                email_with_test_tag = "#{user[:email]}#{(test || {})[:tag]}"
                server_tag = "#{user[:server_tag]}#{(test || {})[:tag]}"
                fs_tag = fs_tag_for_email(email_with_test_tag)
                if running_servers.include?(fs_tag)
                    f.puts <<~END_OF_STRING
                    location /#{server_tag}/ {
                        if ($cookie_hs_server_sid != "#{user[:server_sid]}") {
                            return 403;
                        }
                        rewrite ^/#{server_tag}(.*)$ $1 break;
                        proxy_set_header Host $http_host;
                        proxy_set_header Upgrade $http_upgrade;
                        proxy_set_header Connection upgrade;
                        proxy_set_header Accept-Encoding gzip;
                        proxy_pass http://#{running_servers[fs_tag][:ip]}:8443;
                    }
                    END_OF_STRING
                    if test.nil?
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
            end
            f.puts "}"
        end
        STDERR.puts ">>> Sending HUP to nginx to reload nginx config"
        system("docker kill -s HUP workspace_nginx_1")
    end

    def self.convert_image(image_path)
        image_sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
        target_path = "/webcache/#{image_sha1}.webp"
        unless FileUtils.uptodate?(target_path, [image_path])
            STDERR.puts "Copying #{image_path} to cache..."
            FileUtils.cp(image_path, target_path)
        end
        [1024, 512].each do |width|
            target_path_width = "/webcache/#{image_sha1}-#{width}.webp"
            unless FileUtils.uptodate?(target_path_width, [target_path])
                system("magick #{target_path} -resize #{width}x #{target_path_width}")
                if $? != 0
                    STDERR.puts "...conversion of #{image_path} failed!"
                end
            end
        end
        image_sha1
    end

    def self.inject_autotoc(root, options = {})
        options[:inject_numbers] = true unless options.include?(:inject_numbers)
        h2_list = root.css('h2').reject { |x| x['data-autotoc'] == 'ignore' }
        if h2_list.size > 1
            h2_list.each.with_index do |h2, index|
                autotoc_container = Nokogiri::XML::Node.new('div', root)
                autotoc_container['class'] = 'autotoc-container'
                label = h2.text.strip
                if options[:inject_numbers]
                    label = "#{index + 1}. #{h2.text}".strip
                    h2.content = label
                end
                autotoc_container['data-label'] = label
                h2.add_previous_sibling(autotoc_container)
                autotoc_container.add_child(h2)
                p = autotoc_container.next_element
                while p && p.name != 'h2'
                    pnext = p.next_element
                    autotoc_container.add_child(p)
                    p = pnext
                end
            end
        end
    end

    def self.parse_content
        STDERR.puts "Parsing content..."
        hyphenation_map = {}
        File.read('/src/content/hyphenation.txt').split("\n").each do |line|
            line.strip!
            hyphenation_map[line.gsub('-', '')] = line.gsub('-', '&shy;')
        end
        sections = YAML.load(File.read('/src/content/sections.yaml'))
        @@section_order = sections.map { |section| section['key'] }
        @@sections = {}
        paths = []
        seen_paths = Set.new()
        sections.each do |section|
            @@sections[section['key']] = {}
            section.each_pair do |k, v|
                @@sections[section['key']][k.to_sym] = v
            end
            if @@sections[section['key']][:description]
                hyphenation_map.each_pair do |a, b|
                    @@sections[section['key']][:description].gsub!(a, b)
                end
            end
            @@sections[section['key']][:entries] = []
            (section['entries'] || []).each do |path|
                dev_only = path[0] == '.'
                path = path.sub(/^\./, '')
                seen_paths << path
                original_path = Dir["/src/content/#{path}/*.md"].reject { |x| x.include?('+') }.first
                paths << {:section => section['key'], :path => path, :original_path => original_path, :dev_only => dev_only}
            end
        end
        Dir['/src/content/**/+*.md'].each do |path|
            original_path = path
            path = File.basename(path)
            next if path.include?('/')
            next unless path.include?('+')
            path = path.sub('+', '')
            unless seen_paths.include?(path)
                STDERR.puts "Got custom path: #{path} (#{original_path})"
                paths << {:section => 'misc', :path => path, :dev_only => false, :original_path => original_path, :extra => true}
                seen_paths << path
            end
        end

        @@kenney = {}
        Dir['/src/content/anaglyph/kenney/*/*.webp'].sort.each do |path|
            kit = path.split('/').last(2).first
            model = path.split('/').last(2).last.sub('.webp', '')
            @@kenney[kit] ||= []
            @@kenney[kit] << model
        end

        @@kenney.keys.each do |kit|
            paths << {:section => 'misc', :path => kit, :original_path => "/src/content/anaglyph/#{kit}.md", :dev_only => false, :kenney => true, :extra => true}
        end

        @@content = {}

        redcarpet = Redcarpet::Markdown.new(Redcarpet::Render::HTML, {:fenced_code_blocks => true})
        @@parse_content_count ||= 0
        @@parse_content_count += 1
        paths.each do |entry|
            section = entry[:section]
            path = entry[:original_path]
            next if @@parse_content_count > 1 && entry[:kenney]
            markdown = nil
            unless entry[:kenney]
                next unless path
                markdown = File.read(path)
                markdown.gsub!(/_include_file\(([^)]+)\)/) do |match|
                    options = $1.split(',').map { |x| x.strip }
                    StringIO.open do |io|
                        io.puts "```#{options[1]}_lineno"
                        io.puts File.read(File.join(File.dirname(path), options[0]))
                        io.puts "```"
                        io.string
                    end
                end
            else
                kit = entry[:path]
                markdown = StringIO.open do |io|
                    title = kit.split('-').map { |x| x.capitalize}. join(' ')
                    io.puts "# #{title}"
                    io.puts
                    io.puts <<~EOS
                        Um das #{title} verwenden zu können, musst du es erst installieren. Öffne dazu ein Terminal, indem du <span class='key'>Strg</span><span class='key'>J</span> drückst. Führe dann den folgenden Befehl aus:

                        ```bash
                        ./download.rb #{kit}
                        ```

                        <div class='hint'>
                            Hinweis: Achte auf die genaue Schreibweise des Befehls.
                        </div>

                        Du kannst dann mit dem Befehl `model` ein Modell zu deiner Szene hinzufügen, also z. B.:

                        ```ini
                        model = #{kit}/#{@@kenney[kit].first}
                        ```

                        <div class='hint'>
                            Achtung: Vergiss nicht, den Namen des Kits (<code>#{kit}/</code>) vor dem Modellnamen anzugeben!
                        </div>

                        Die folgenden Modelle stehen zur Auswahl:

                        <div class='kenney-gallery'>
                    EOS
                    @@kenney[kit].each do |model|
                        io.puts "<div><img src='kenney/#{kit}/#{model}.webp' data-noconvert='true'><div>#{model}</div></div>"
                    end
                    io.puts "</div>"
                    io.string
                end
            end
            hyphenation_map.each_pair do |a, b|
                markdown.gsub!(a, b)
            end
            slug = File.basename(path, '.md').sub(/^[0-9]+\-/, '').sub('+', '')
            html = redcarpet.render(markdown)
            root = Nokogiri::HTML(html)
            meta = root.css('.meta').first
            if meta
                meta = YAML.load(meta)
            end

            root.css('img').each do |img|
                src = img.attr('src')
                image_path = File.join(File.dirname(path), src)
                next unless File.exist?(image_path)
                if img.attr('data-noconvert')
                    sha1 = Digest::SHA1.hexdigest(image_path)
                    system("cp -pu \"#{image_path}\" /webcache/#{sha1}.#{image_path.split('.').last}")
                    img['src'] = "/cache/#{sha1}.#{image_path.split('.').last}"
                else
                    sha1 = convert_image(image_path)
                    img['src'] = "/cache/#{sha1}.webp"
                end
                if img.classes.include?('full')
                    img.wrap("<div class='scroll-x'>")
                end
            end
            root.css('video').each do |video|
                src = video.attr('src')
                image_path = File.join(File.dirname(path), src)
                next unless File.exist?(image_path)
                sha1 = Digest::SHA1.hexdigest(File.read(image_path))[0, 16]
                system("cp -pu \"#{image_path}\" /webcache/#{sha1}.mp4")
                video['src'] = "/cache/#{sha1}.mp4"
            end
            root.css('a').each do |a|
                href = a.attr('href')
                if href.index('https://') == 0
                    a['target'] = '_blank'
                end
            end
            root.css('pre').each do |pre|
                code = pre.css('code').first
                next if code.nil?
                language = code.attr('class')
                lineno = false
                if language =~ /_lineno$/
                    lineno = true
                    language = language.sub(/_lineno$/, '')
                end
                formatter = if lineno
                    Rouge::Formatters::HTMLTable.new(Rouge::Formatters::HTML.new)
                    # Rouge::Formatters::HTMLLineTable.new(Rouge::Formatters::HTML.new)
                else
                    Rouge::Formatters::HTML.new
                end
                lexer = nil
                case language
                when 'bash'
                    lexer = Rouge::Lexers::Shell.new
                when 'basic'
                    lexer = Rouge::Lexers::VisualBasic.new
                when 'c'
                    lexer = Rouge::Lexers::C.new
                when 'clisp'
                    lexer = Rouge::Lexers::CommonLisp.new
                when 'cpp'
                    lexer = Rouge::Lexers::Cpp.new
                when 'cs'
                    lexer = Rouge::Lexers::CSharp.new
                when 'css'
                    lexer = Rouge::Lexers::CSS.new
                when 'dart'
                    lexer = Rouge::Lexers::Dart.new
                when 'erlang'
                    lexer = Rouge::Lexers::Erlang.new
                when 'fortran'
                    lexer = Rouge::Lexers::Fortran.new
                when 'go'
                    lexer = Rouge::Lexers::Go.new
                when 'html'
                    lexer = Rouge::Lexers::HTML.new
                when 'ini'
                    lexer = Rouge::Lexers::INI.new
                when 'java'
                    lexer = Rouge::Lexers::Java.new
                when 'js'
                    lexer = Rouge::Lexers::Javascript.new
                when 'json'
                    lexer = Rouge::Lexers::JSON.new
                when 'lua'
                    lexer = Rouge::Lexers::Lua.new
                when 'nasm'
                    lexer = Rouge::Lexers::Nasm.new
                when 'pascal'
                    lexer = Rouge::Lexers::Pascal.new
                when 'python'
                    lexer = Rouge::Lexers::Python.new
                when 'ruby'
                    lexer = Rouge::Lexers::Ruby.new
                when 'rust'
                    lexer = Rouge::Lexers::Rust.new
                when 'smalltalk'
                    lexer = Rouge::Lexers::Smalltalk.new
                when 'sql'
                    lexer = Rouge::Lexers::SQL.new
                when 'svelte'
                    lexer = Rouge::Lexers::Svelte.new
                when 'text'
                    lexer = Rouge::Lexers::PlainText.new
                end
                next if lexer.nil?
                pre.content = ''
                # if lineno
                #     pre << "<div style='border-bottom: 1px solid white;display: inline-table;width: 100%;margin-bottom: 0.5em;padding-bottom: 0.5em;font-style: italic;font-weight: normal;'>HelloWorld.java (124 Bytes)</div>"
                # end
                pre << formatter.format(lexer.lex(code.text))
            end

            inject_autotoc(root)

            html = root.to_html
            # root.css('h2, h3, h4, h5, h6').each do |e|
            #     if slug == 'c'
            #         level = e.name.sub('h', '').to_i
            #         STDERR.puts "#{'  ' * (level - 2)}- #{e.text}"
            #     end
            # end
            meta = root.css('.meta').first
            @@content[slug] = {
                :html => html,
                :dev_only => entry[:dev_only],
            }
            unless entry[:extra]
                @@sections[section][:entries] << slug
                if meta
                    meta = YAML.load(meta)
                    if meta['image']
                        parts = meta['image'].split(':')
                        sha1 = convert_image(File.join(File.dirname(path), parts[0]))
                        @@content[slug][:image] = "/cache/#{sha1}.webp"
                        @@content[slug][:image_x] = (parts[1] || '50').to_i
                        @@content[slug][:image_y] = (parts[2] || '50').to_i
                        @@content[slug][:needs_contrast] = meta['needs_contrast']
                    end
                end
                begin
                    @@content[slug][:title] = root.css('h1').first.to_s.sub('<h1>', '').sub('</h1>', '').strip
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

    def self.load_invitations
        @@invitations = {}
        @@user_groups = {}
        @@user_group_order = []

        current_group = 'Administrator'

        ADMIN_USERS.each do |email|
            @@user_group_order << current_group unless @@user_group_order.include?(current_group)
            @@invitations[email] = { :group => current_group, :name => email }
            @@user_groups[current_group] ||= []
            @@user_groups[current_group] << email
        end

        current_group = '(keine Gruppe)'
        group_admins = {}
        Dir['/invitations/*.txt'].sort.each do |path|
            next if File.basename(path) == '_template.txt'
            File.open(path) do |f|
                f.each_line do |line|
                    next if line.strip.empty?
                    next if line.strip[0] == '#'
                    if line[0] == '>'
                        current_group = line[1, line.size - 1].strip
                        group_admins[current_group] ||= []
                        @@user_group_order << current_group unless @@user_group_order.include?(current_group)
                    elsif line[0] == '+'
                        group_admins[current_group] << line[1, line.size - 1].strip.delete_prefix('<').delete_suffix('>')
                    else
                        parts = line.strip.split(' ')
                        email = parts.last.delete_prefix('<').delete_suffix('>').downcase
                        unless @@invitations[email]
                            @@user_groups[current_group] ||= []
                            @@user_groups[current_group] << email
                            @@invitations[email] = { :group => current_group }
                        end
                        if parts.size > 1
                            name = parts[0, parts.size - 1].join(' ')
                            @@invitations[email][:name] = name
                        else
                            @@invitations[email][:name] = email
                        end
                    end
                end
            end
        end
        @@teachers = {}
        group_admins.each_pair do |group, emails|
            emails.each do |email|
                @@teachers[email] ||= Set.new()
                @@teachers[email] << group
            end
        end
    end

    def self.prepare_downloads()
        unless File.exist?("/dl/working-with-files.tar.gz")
            STDERR.puts "Preparing /dl/working-with-files.tar.bz2..."
            FileUtils.rm_rf('/dl/working-with-files')
            FileUtils.mkpath('/dl/working-with-files')
            system("wget -O /dl/working-with-files/alice.txt https://www.gutenberg.org/cache/epub/11/pg11.txt")
            system("wget -O /dl/working-with-files/stallman.jpg https://upload.wikimedia.org/wikipedia/commons/c/c2/Richard_Stallman_at_Marlboro_College.jpg")
            system("wget -O /dl/working-with-files/jay.webm https://upload.wikimedia.org/wikipedia/commons/transcoded/7/75/Jay_Feeding.webm/Jay_Feeding.webm.360p.vp9.webm")
            system("wget -O /dl/working-with-files/zork.zip https://github.com/devshane/zork/archive/refs/heads/master.zip")
            system("wget -O /dl/working-with-files/music-releases.tar.bz2 https://github.com/specht/workspace-files/raw/main/music-releases.tar.bz2")
            system("echo 'Nothing to see here, just a hidden file!' > /dl/working-with-files/.hidden")
            system("tar cvzf /dl/working-with-files.tar.gz -C /dl working-with-files")
            FileUtils.rm_rf('/dl/working-with-files')
        end
    end

    configure do
        CONSTRAINTS_LIST = [
            'File/sha1',
            'Test/tag',
            'User/email',
            'User/server_tag',
            'User/share_tag',
            'LoginRequest/tag',
            'Session/sid',
            'TIC80File/path',
            'TIC80Dir/path',
            'Database/name'
        ]
        INDEX_LIST = [
            'Test/running'
        ]

        setup = SetupDatabase.new()

        setup.wait_for_neo4j()
        @@clients = {}
        @@email_for_client_id = {}
        @@client_ids_for_email = {}
        @@threads_for_client_id = {}
        $neo4j.setup_constraints_and_indexes(CONSTRAINTS_LIST, INDEX_LIST)
        self.refresh_nginx_config()
        self.parse_content()
        self.load_invitations()
        self.prepare_downloads()

        @@brand_header = ''
        begin
            @@brand_header = File.read('/brand/header.html')
        rescue
        end

        Thread.new do
            loop do
                system("ruby housekeeping.rb")
                sleep 3600
            end
        end
    end

    before '*' do
        @session_user = nil
        if request.cookies.include?('hs_sid')
            sid = request.cookies['hs_sid']
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
                                results.first['u'].each_pair do |k, v|
                                    next if @session_user.include?(k.to_sym)
                                    @session_user[k.to_sym] = v
                                end
                                @session_user[:show_workspace] = true unless @session_user.include?(:show_workspace)
                                # # set server_sid cookie if it's not set or out of date
                                expires = Time.new + 3600 * 24 * 365
                                [:hs_server_sid].each do |key|
                                    if request.cookies[key.to_s] != results.first['u'][key.to_s.sub('hs_', '').to_sym]
                                        response.set_cookie(key.to_s,
                                            :domain => ".#{WEBSITE_HOST}",
                                            :value => results.first['u'][key.to_s.sub('hs_', '').to_sym],
                                            :expires => expires,
                                            :path => '/',
                                            :httponly => true,
                                            :secure => DEVELOPMENT ? false : true
                                        )
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

    def teacher_logged_in?
        admin_logged_in? || (user_logged_in? && @@teachers.include?(@session_user[:email]))
    end

    def admin_logged_in?
        user_logged_in? && ADMIN_USERS.include?(@session_user[:email])
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
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:email])
        email = data[:email].downcase.strip

        unless @@invitations.include?(email)
            candidates = @@invitations.keys.select do |x|
                x[0, email.size] == email
            end
            if candidates.size == 1
                email = candidates.first
            end
        end
        unless @@invitations.include?(email)
            respond(:error => 'no_invitation_found')
        end
        assert(@@invitations.include?(email), 'no_invitation_found')

        tag = RandomTag::generate(12)
        srand(Digest::SHA2.hexdigest(LOGIN_CODE_SALT).to_i + (Time.now.to_f * 1000000).to_i)
        random_code = (0..5).map { |x| rand(10).to_s }.join('')
        random_code = '123456' if DEVELOPMENT

        # create user node if it doesn't already exist
        user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email)['n']
            MERGE (n:User {email: $email})
            RETURN n;
        END_OF_QUERY
        unless user[:name]
            name = @@invitations[email][:name]
            user = neo4j_query_expect_one(<<~END_OF_QUERY, :email => email, :name => name)['n']
                MATCH (n:User {email: $email})
                SET n.name = $name
                RETURN n;
            END_OF_QUERY
        end

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

        STDERR.puts "Sending login code #{random_code} to #{email}... go to /l/#{tag}/#{random_code} to log in."
        deliver_mail do
            to email
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
        sid = request.cookies['hs_sid']
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
            result[:ip] = inspect.first['NetworkSettings']['Networks']['bridge']['IPAddress']
        end
        result
    end

    def self.gen_password_for_email(email, salt)
        chars = 'BCDFGHJKMNPQRSTVWXYZ23456789'.split('')
        sha2 = Digest::SHA256.new()
        sha2 << salt
        sha2 << email
        srand(sha2.hexdigest.to_i(16))
        password = ''
        8.times do
            c = chars.sample.dup
            c.downcase! if [0, 1].sample == 1
            password += c
        end
        password += '-'
        4.times do
            c = chars.sample.dup
            c.downcase! if [0, 1].sample == 1
            password += c
        end
        password
    end

    def init_mysql(email)
        mysql_password = Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT)
        login = email.split('@').first.downcase
        STDERR.puts "Setting up MySQL user #{login} with database #{login}"
        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts "CREATE USER IF NOT EXISTS '#{login}'@'%' identified by '#{mysql_password}';"
            stdin.puts "CREATE DATABASE IF NOT EXISTS `#{login}`;"
            stdin.puts "GRANT ALL ON `#{login}`.* TO '#{login}'@'%';"
            stdin.puts "FLUSH PRIVILEGES;"
            stdin.close
            wait_thr.value
        end
    end

    def reset_mysql(email)
        login = email.split('@').first.downcase
        STDERR.puts "Removing database for MySQL user #{login}"
        Open3.popen2("docker exec -i workspace_mysql_1 mysql --user=root --password=#{MYSQL_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
            stdin.puts "DROP DATABASE IF EXISTS `#{login}`;"
            stdin.close
            wait_thr.value
        end
        init_mysql(email)
    end

    def init_postgres(email)
        postgres_password = Main.gen_password_for_email(email, POSTGRES_PASSWORD_SALT)
        login = email.split('@').first.downcase
        STDERR.puts "Setting up Postgres user #{login} with database #{login}"

        Open3.popen2("docker exec -i -e PGPASSWORD=#{POSTGRES_ROOT_PASSWORD} workspace_postgres_1 psql --user=postgres") do |stdin, stdout, wait_thr|
            stdin.puts <<~END_OF_STRING
                CREATE USER \"#{login}\" WITH ENCRYPTED PASSWORD '#{postgres_password}';
                CREATE DATABASE \"#{login}\";
                ALTER DATABASE \"#{login}\" OWNER TO \"#{login}\";
                REVOKE ALL PRIVILEGES ON DATABASE \"#{login}\" FROM public;
            END_OF_STRING
            stdin.close
            wait_thr.value
        end
    end

    def reset_postgres(email)
        STDERR.puts "Removing database for Postgres user #{email}"
        login = email.split('@').first.downcase
        Open3.popen2("docker exec -i -e PGPASSWORD=#{POSTGRES_ROOT_PASSWORD} workspace_postgres_1 psql --user=postgres") do |stdin, stdout, wait_thr|
            stdin.puts <<~END_OF_STRING
                DROP DATABASE IF EXISTS "#{login}" WITH (FORCE);
            END_OF_STRING
            stdin.close
            wait_thr.value
        end
        init_postgres(email)
    end

    # def init_neo4j(email)
    #     neo4j_password = Main.gen_password_for_email(email, NEO4J_PASSWORD_SALT)
    #     login = email.split('@').first.downcase
    #     database = login.gsub('.', '_')
    #     STDERR.puts "Setting up Neo4j user #{login} with database #{login}"
    #     Open3.popen2("docker exec -i workspace_neo4j_user_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
    #         stdin.puts <<~END_OF_STRING
    #             CREATE DATABASE #{database} IF NOT EXISTS;
    #             CREATE USER #{login} IF NOT EXISTS SET PLAINTEXT PASSWORD '#{neo4j_password}' CHANGE NOT REQUIRED
    #             SET HOME DATABASE #{database};
    #             GRANT ROLE architect TO #{login};
    #         END_OF_STRING
    #         stdin.close
    #         wait_thr.value
    #     end
    # end

    # def reset_neo4j(email)
    #     login = email.split('@').first.downcase
    #     database = login.gsub('.', '_')
    #     Open3.popen2("docker exec -i workspace_neo4j_user_1 bin/cypher-shell -u neo4j -p #{NEO4J_ROOT_PASSWORD}") do |stdin, stdout, wait_thr|
    #         stdin.puts <<~END_OF_STRING
    #             DROP DATABASE #{database} IF EXISTS;
    #         END_OF_STRING
    #         stdin.close
    #         wait_thr.value
    #     end
    #     init_neo4j(email)
    # end

    def start_server(email, test_tag = nil)
        email_with_test_tag = "#{email}#{test_tag}"
        container_name = fs_tag_for_email(email_with_test_tag)

        STDERR.puts ">>> Starting server with email #{email_with_test_tag} and container name #{container_name}"

        system("mkdir -p /user/#{container_name}/config")
        system("mkdir -p /user/#{container_name}/workspace")
        if File.exist?("/user/#{container_name}/workspace/.bashrc")
            contents = File.read("/user/#{container_name}/workspace/.bashrc")
            unless contents.include?('GEM_HOME')
                system("echo 'export GEM_HOME=\"$HOME/.gem\"' >> /user/#{container_name}/workspace/.bashrc")
            end
        end
        # touch this file so that housekeeping won't shut down the server immediately
        File.open("/user/#{container_name}/workspace/.hackschule", 'w') do |f|
            f.puts "https://youtu.be/Akaa9xHaw7E"
        end
        system("touch /user/#{container_name}/workspace/.hackschule")
        
        unless File.exist?("/user/#{container_name}/workspace/.my.cnf")
            File.open("/user/#{container_name}/workspace/.my.cnf", 'w') do |f|
                f.puts <<~END_OF_STRING
                    [client]
                    user = #{email.split('@').first.downcase}
                    password = #{Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT)}
                    host = mysql
                    database = #{email.split('@').first.downcase}
                    port = 3306
                END_OF_STRING
            end
        end
        unless File.exist?("/user/#{container_name}/workspace/.myclirc")
            File.open("/user/#{container_name}/workspace/.myclirc", 'w') do |f|
                f.puts <<~END_OF_STRING
                    [main]

                    # Enables context sensitive auto-completion. If this is disabled the all
                    # possible completions will be listed.
                    smart_completion = True

                    # Multi-line mode allows breaking up the sql statements into multiple lines. If
                    # this is set to True, then the end of the statements must have a semi-colon.
                    # If this is set to False then sql statements can't be split into multiple
                    # lines. End of line (return) is considered as the end of the statement.
                    multi_line = True

                    # Destructive warning mode will alert you before executing a sql statement
                    # that may cause harm to the database such as "drop table", "drop database"
                    # or "shutdown".
                    destructive_warning = True

                    # log_file location.
                    log_file = ~/.mycli.log

                    # Default log level. Possible values: "CRITICAL", "ERROR", "WARNING", "INFO"
                    # and "DEBUG". "NONE" disables logging.
                    log_level = INFO

                    # Log every query and its results to a file. Enable this by uncommenting the
                    # line below.
                    # audit_log = ~/.mycli-audit.log

                    # Timing of sql statements and table rendering.
                    timing = True

                    # Beep after long-running queries are completed; 0 to disable.
                    beep_after_seconds = 0

                    # Table format. Possible values: ascii, double, github,
                    # psql, plain, simple, grid, fancy_grid, pipe, orgtbl, rst, mediawiki, html,
                    # latex, latex_booktabs, textile, moinmoin, jira, vertical, tsv, csv.
                    # Recommended: ascii
                    table_format = double

                    # Syntax coloring style. Possible values (many support the "-dark" suffix):
                    # manni, igor, xcode, vim, autumn, vs, rrt, native, perldoc, borland, tango, emacs,
                    # friendly, monokai, paraiso, colorful, murphy, bw, pastie, paraiso, trac, default,
                    # fruity.
                    # Screenshots at http://mycli.net/syntax
                    # Can be further modified in [colors]
                    syntax_style = default

                    # Keybindings: Possible values: emacs, vi.
                    # Emacs mode: Ctrl-A is home, Ctrl-E is end. All emacs keybindings are available in the REPL.
                    # When Vi mode is enabled you can use modal editing features offered by Vi in the REPL.
                    key_bindings = emacs

                    # Enabling this option will show the suggestions in a wider menu. Thus more items are suggested.
                    wider_completion_menu = False

                    # MySQL prompt
                    # \\D - The full current date
                    # \\d - Database name
                    # \\h - Hostname of the server
                    # \\m - Minutes of the current time
                    # \\n - Newline
                    # \\P - AM/PM
                    # \\p - Port
                    # \\R - The current time, in 24-hour military time (0-23)
                    # \\r - The current time, standard 12-hour time (1-12)
                    # \\s - Seconds of the current time
                    # \\t - Product type (Percona, MySQL, MariaDB, TiDB)
                    # \\A - DSN alias name (from the [alias_dsn] section)
                    # \\u - Username
                    # \\x1b[...m - insert ANSI escape sequence
                    prompt = '\\d> '
                    prompt_continuation = '->'

                    # Skip intro info on startup and outro info on exit
                    less_chatty = False

                    # Use alias from --login-path instead of host name in prompt
                    login_path_as_host = False

                    # Cause result sets to be displayed vertically if they are too wide for the current window,
                    # and using normal tabular format otherwise. (This applies to statements terminated by ; or \G.)
                    auto_vertical_output = False

                    # keyword casing preference. Possible values "lower", "upper", "auto"
                    keyword_casing = upper

                    # disabled pager on startup
                    enable_pager = True

                    # Choose a specific pager
                    pager = 'less -S -F'

                    # Custom colors for the completion menu, toolbar, etc.
                    [colors]
                    completion-menu.completion.current = 'bg:#ffffff #000000'
                    completion-menu.completion = 'bg:#008888 #ffffff'
                    completion-menu.meta.completion.current = 'bg:#44aaaa #000000'
                    completion-menu.meta.completion = 'bg:#448888 #ffffff'
                    completion-menu.multi-column-meta = 'bg:#aaffff #000000'
                    scrollbar.arrow = 'bg:#003333'
                    scrollbar = 'bg:#00aaaa'
                    selected = '#ffffff bg:#6666aa'
                    search = '#ffffff bg:#4444aa'
                    search.current = '#ffffff bg:#44aa44'
                    bottom-toolbar = 'bg:#222222 #aaaaaa'
                    bottom-toolbar.off = 'bg:#222222 #888888'
                    bottom-toolbar.on = 'bg:#222222 #ffffff'
                    search-toolbar = 'noinherit bold'
                    search-toolbar.text = 'nobold'
                    system-toolbar = 'noinherit bold'
                    arg-toolbar = 'noinherit bold'
                    arg-toolbar.text = 'nobold'
                    bottom-toolbar.transaction.valid = 'bg:#222222 #00ff5f bold'
                    bottom-toolbar.transaction.failed = 'bg:#222222 #ff005f bold'

                    # style classes for colored table output
                    output.header = "#00ff5f bold"
                    output.odd-row = ""
                    output.even-row = ""
                    output.null = "#808080"

                    # SQL syntax highlighting overrides
                    # sql.comment = 'italic #408080'
                    # sql.comment.multi-line = ''
                    # sql.comment.single-line = ''
                    # sql.comment.optimizer-hint = ''
                    # sql.escape = 'border:#FF0000'
                    # sql.keyword = 'bold #008000'
                    # sql.datatype = 'nobold #B00040'
                    # sql.literal = ''
                    # sql.literal.date = ''
                    # sql.symbol = ''
                    # sql.quoted-schema-object = ''
                    # sql.quoted-schema-object.escape = ''
                    # sql.constant = '#880000'
                    # sql.function = '#0000FF'
                    # sql.variable = '#19177C'
                    # sql.number = '#666666'
                    # sql.number.binary = ''
                    # sql.number.float = ''
                    # sql.number.hex = ''
                    # sql.number.integer = ''
                    # sql.operator = '#666666'
                    # sql.punctuation = ''
                    # sql.string = '#BA2121'
                    # sql.string.double-quouted = ''
                    # sql.string.escape = 'bold #BB6622'
                    # sql.string.single-quoted = ''
                    # sql.whitespace = ''

                    # Favorite queries.
                    [favorite_queries]

                    # Use the -d option to reference a DSN.
                    # Special characters in passwords and other strings can be escaped with URL encoding.
                    [alias_dsn]
                    # example_dsn = mysql://[user[:password]@][host][:port][/dbname]
                END_OF_STRING
            end
        end
        STDERR.puts ">>> Getting server state"

        state = get_server_state(container_name)
        STDERR.puts ">>> Server state is #{state.to_yaml}"

        unless state[:running]
            config_path = "/user/#{container_name}/workspace/.local/share/code-server/User/settings.json"
            unless File.exist?(config_path)
                FileUtils.mkpath(File.dirname(config_path))
                File.open(config_path, 'w') do |f|
                    config = {}
                    config['files.exclude'] ||= {}
                    config['files.exclude']['**/.*'] = true
                    config['files.autoSave']||= "off"
                    config["window.menuBarVisibility"] ||= "classic"
                    config['telemetry.telemetryLevel'] ||= 'off'
                    if test_tag
                        config['workbench.colorTheme'] = 'Tomorrow Night Blue'
                    end
                    f.write config.to_json
                end
            end
            if test_tag
                config_path = "/user/#{container_name}/workspace/.local/share/code-server/coder.json"
                unless File.exist?(config_path)
                    FileUtils.mkpath(File.dirname(config_path))
                    File.open(config_path, 'w') do |f|
                        config = {}
                        config['query'] ||= {}
                        config['query']['folder'] = '/workspace'
                        f.write config.to_json
                    end
                end
            end
            # {
            #     "query": {
            #       "folder": "/workspace"
            #     }
            #   }
            system("chown -R 1000:1000 /user/#{container_name}")

            db_email = email
            if test_tag
                db_email = "#{email}-#{test_tag}"
            end
            STDERR.puts ">>> Initializing MySQL"
            init_mysql(db_email)
            STDERR.puts ">>> Initializing Postgres"
            init_postgres(db_email)
            # init_neo4j(db_email)

            if test_tag
                test_init_mark_path = "/user/#{container_name}/workspace/.test_init"
                unless File.exist?(test_init_mark_path)
                    sha1 = neo4j_query_expect_one(<<~END_OF_QUERY, {:test_tag => test_tag})['f.sha1']
                        MATCH (t:Test {tag: $test_tag})-[:USES]->(f:File)
                        RETURN f.sha1;
                    END_OF_QUERY
                    # unpack files from archive
                    system("tar xf /internal/test_archives/#{sha1} -C /user/#{container_name}/workspace")
                    File.open(test_init_mark_path, 'w') do |f|
                    end
                    # check if we have a config file in the archive
                    if File.exist?("/user/#{container_name}/workspace/.workspace/config.yaml")
                        config = YAML.load(File.read("/user/#{container_name}/workspace/.workspace/config.yaml"))
                        if config['vscode_config']
                            config_path = "/user/#{container_name}/workspace/.local/share/code-server/User/settings.json"
                            vscode_config = JSON.parse(File.read(config_path))
                            vscode_config.merge!(config['vscode_config'])
                            File.open(config_path, 'w') do |f|
                                f.write vscode_config.to_json
                            end
                        end
                    end
                end
            end

            network_name = "bridge"
            STDERR.puts ">>> Getting IP addresses for mysql and postgres..."
            mysql_ip = `docker inspect workspace_mysql_1`.split('"IPAddress": "')[1].split('"')[0]
            postgres_ip = `docker inspect workspace_postgres_1`.split('"IPAddress": "')[1].split('"')[0]
            STDERR.puts ">>> MySQL running #{mysql_ip}, Postgres running at #{postgres_ip}"
            # neo4j_ip = `docker inspect workspace_neo4j_user_1`.split('"IPAddress": "')[1].split('"')[0]
            login = email.split('@').first.downcase
            mysql_login = db_email.split('@').first.downcase
            STDERR.puts ">>> Login is #{login}, MySQL login is #{mysql_login}"
            command = "docker run --add-host=mysql:#{mysql_ip} --add-host=postgres:#{postgres_ip} --cpus=2 -d --rm -e PUID=1000 -e GUID=1000 -e TZ=Europe/Berlin -e PWA_APPNAME=\"Workspace\" -e DEFAULT_WORKSPACE=/workspace -e MYSQL_HOST=\"mysql\" -e MYSQL_USER=\"#{mysql_login}\" -e MYSQL_PASSWORD=\"#{Main.gen_password_for_email(db_email, MYSQL_PASSWORD_SALT)}\" -e MYSQL_DATABASE=\"#{mysql_login}\" -e POSTGRES_HOST=\"postgres\" -e POSTGRES_USER=\"#{login}\" -e POSTGRES_PASSWORD=\"#{Main.gen_password_for_email(email, POSTGRES_PASSWORD_SALT)}\" -e POSTGRES_DATABASE=\"#{login}\"  -e NEO4J_HOST=\"neo4j\" -e NEO4J_USER=\"#{mysql_login}\" -e NEO4J_PASSWORD=\"#{Main.gen_password_for_email(mysql_login, NEO4J_PASSWORD_SALT)}\" -e NEO4J_DATABASE=\"#{mysql_login.gsub('.', '_')}\" -v #{PATH_TO_HOST_DATA}/user/#{container_name}/config:/config -v #{PATH_TO_HOST_DATA}/user/#{container_name}/workspace:/workspace --network #{network_name} #{test_tag ? '-v /dev/null:/etc/resolv.conf:ro' : ''} --name hs_code_#{container_name} hs_code_server"
            STDERR.puts ">>> Command:\n#{command}"
            system(command)
            Main.refresh_nginx_config()
        end

        return "#{@session_user[:server_tag]}#{test_tag}"
    end

    def stop_server(email)
        container_name = fs_tag_for_email(email)

        system("docker kill hs_code_#{container_name}")

        Main.refresh_nginx_config()
    end

    post '/api/start_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_mysql(email)
        respond(:yay => 'sure')
    end

    post '/api/reset_mysql' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_mysql(email)
        respond(:yay => 'sure')
    end

    post '/api/start_postgres' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_postgres(email)
        respond(:yay => 'sure')
    end

    post '/api/reset_postgres' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_postgres(email)
        respond(:yay => 'sure')
    end

    post '/api/start_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        init_neo4j(email)
        respond(:yay => 'sure')
    end

    post '/api/reset_neo4j' do
        assert(user_logged_in?)
        email = @session_user[:email]
        reset_neo4j(email)
        respond(:yay => 'sure')
    end

    post '/api/start_server' do
        assert(user_logged_in?)
        data = parse_request_data(:optional_keys => [:test_tag])

        email = @session_user[:email]
        server_tag = start_server(email, data[:test_tag])

        respond(:yay => 'sure', :server_tag => server_tag)
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
        assert(teacher_logged_in?)
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

        expires = Time.new + 3600 * 24 * 365
        response.set_cookie('hs_watch_tag',
            :domain => ".#{WEBSITE_HOST}",
            :value => watch_tag,
            :expires => expires,
            :path => "/", #"/#{fs_tag_for_email(email)}",
            :httponly => true,
            :secure => DEVELOPMENT ? false : true)
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
        html = StringIO.open do |io|
            running_tests = my_running_tests()
            if running_tests.empty?
                @@section_order.each do |section_key|
                    section = @@sections[section_key]
                    next if section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end.empty?
                    io.puts "<h2><div class='squircle'><img src='#{section[:icon]}'></div> #{section[:label]}</h2>"
                    if section[:description]
                        io.puts "<p style='margin-top: -1em; margin-bottom: 1em;'>#{section[:description]}</p>"
                    end
                    io.puts "<div class='row'>"
                    entries = section[:entries].reject do |entry|
                        (!DEVELOPMENT) && @@content[entry][:dev_only]
                    end
                    entries.each.with_index do |slug, index|
                        hor2 = ((entries.length % 2) == 1) && (index == entries.length - 1)
                        hor3 = false
                        if entries.length % 3 == 1
                            hor3 = index >= entries.length - 4
                        elsif entries.length % 3 == 2
                            hor3 = index >= entries.length - 2
                        end

                        content = @@content[slug]
                        io.puts "<div class='hscol #{hor2 ? 'hor2' : ''} #{hor3 ? 'hor3' : ''}'>"
                        io.puts "<a href='/#{slug}' class='tutorial_card3'>"
                        io.puts "<h4>#{content[:title]}</h4>"
                        io.puts "<div>"
                        additional_classes = []
                        if content[:needs_contrast] == 'light'
                            additional_classes << 'dark-only-bg-contrast-light'
                        end
                        io.puts "<img class='#{additional_classes.join(' ')}' src='#{(content[:image] || '/images/white.webp').sub('.webp', '-1024.webp')}' style='object-position: #{content[:image_x]}% #{content[:image_y]}%;'>"
                        io.puts "<div class='shade'></div>"
                        io.puts "<div class='card-content'>"
                        io.puts "#{content[:dev_only] ? '<span class="badge badge-sm bg-danger">dev</span> ' : ''}<h4>#{content[:title]}</h4>"
                        io.puts "<div class='abstract'>#{content[:abstract]}</div>"
                        io.puts "</div>"
                        io.puts "</div>"
                        io.puts "</a>"
                        io.puts "</div>"
                    end
                    io.puts "</div>"
                end
            else
                io.puts "<div style='text-align: center;'>"
                io.puts "<h2>Leistungsüberprüfung</h2>"
                io.puts "<p style='text-align: center;'>"
                io.puts "Momentan läuft eine Leistungsüberprüfung. Klicke auf den entsprechenden Button, um den dazugehörigen Workspace zu öffnen."
                io.puts "</p>"
                running_tests.each do |entry|
                    io.puts "<p style='text-align: center;'><button class='btn btn-success bu-launch-test' data-tag='#{entry[:tag]}'><i class='bi bi-code-slash'></i>Leistungsüberprüfung bei #{entry[:teacher]}</button></p>"
                end
                io.puts "</div>"
            end
            io.string
        end
        doc = Nokogiri::HTML::DocumentFragment.parse(html)
        Main.inject_autotoc(doc, inject_numbers: false)
        doc.to_html
    end

    def print_workspaces()
        assert(teacher_logged_in?)

        # remove all stale login requests
        ts = Time.now.to_i - 60 * 10
        neo4j_query(<<~END_OF_QUERY, {:ts => ts})
            MATCH (l:LoginRequest)
            WHERE COALESCE(l.ts_expiry, 0) < $ts
            DETACH DELETE l;
        END_OF_QUERY

        email_for_tag = {}
        registered_emails = []
        neo4j_query(<<~END_OF_STRING).each do |row|
            MATCH (u:User) RETURN u.email;
        END_OF_STRING
            email = row['u.email']
            email_for_tag[fs_tag_for_email(email)] = email
            registered_emails << email
        end
        du_for_fs_tag = {}
        begin
            du_for_fs_tag = JSON.parse(File.read('/internal/du_for_fs_tag.json'))
        rescue
        end

        STDERR.puts "email_for_tag: #{email_for_tag.to_yaml}"

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
            io.puts "<table class='table table-sm' id='table_admin_workspaces'>"
            active_users = Dir['/user/*'].map do |path|
                path.split('/').last
            end.select do |user_tag|
                user_tag =~ /^[0-9a-z]{16}$/
            end
            active_users = Set.new(active_users)
            @@user_group_order.each do |group|
                sub = StringIO.open do |io2|
                    (@@user_groups[group] || []).each do |email|
                        next unless (@@teachers[@session_user[:email]] || Set.new()).include?(group) || admin_logged_in? || email == @session_user[:email]
                        user_tag = fs_tag_for_email(email)
                        next unless active_users.include?(user_tag)
                        io2.puts "<tr id='tr_hs_code_#{user_tag}'>"
                        io2.puts "<td><code>#{user_tag}</code></td>"
                        io2.puts "<td>#{@@invitations[email_for_tag[user_tag]][:name]}</td>"
                        # io2.puts "<td class='td_ip'>#{(info_for_tag[user_tag] || {})[:ip] || '&ndash;'}</td>"
                        io2.puts "<td class='td_cpu'>&ndash;</td>"
                        io2.puts "<td class='td_ram'>&ndash;</td>"
                        io2.puts "<td class='td_net'>&ndash;</td>"
                        io2.puts "<td class='td_disk'>&ndash;</td>"
                        if du_for_fs_tag[user_tag]
                            io2.puts "<td>#{bytes_to_str(du_for_fs_tag[user_tag] * 1024).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')}</td>"
                        else
                            io2.puts "<td>&ndash;</td>"
                        end
                        io2.puts "<td class='td_pid'>&ndash;</td>"
                        io2.puts "<td><button class='btn btn-sm btn-success bu-open-workspace-as-admin' data-email='#{email_for_tag[user_tag]}'><i class='bi bi-code-slash'></i>&nbsp;Workspace öffnen</button></td>"
                        if admin_logged_in?
                            io2.puts "<td><button class='btn btn-sm btn-warning bu-impersonate' data-email='#{email_for_tag[user_tag]}'><i class='bi bi-person-vcard'></i>&nbsp;Impersonate</button></td>"
                        end
                        io2.puts "</tr>"
                    end
                    io2.string
                end
                unless sub.empty?
                    io.puts "<thead>"
                    io.puts "<tr><th colspan='#{admin_logged_in? ? 10 : 9}' style='background-color: rgba(0,0,0,0); padding: 1em 0;'><h4>#{group}</h4></th></tr>"
                    io.puts "<tr>"
                    io.puts "<th>Tag</th>"
                    io.puts "<th>Name</th>"
                    # io.puts "<th>IP</th>"
                    io.puts "<th style='width: 5.2em;'>CPU</th>"
                    io.puts "<th>RAM</th>"
                    io.puts "<th>Network</th>"
                    io.puts "<th>Disk I/O</th>"
                    io.puts "<th>Storage</th>"
                    io.puts "<th>#PID</th>"
                    io.puts "<th>Workspace</th>"
                    if admin_logged_in?
                        io.puts "<th>Impersonate</th>"
                    end
                    io.puts "</tr>"
                    io.puts "</thead>"
                    io.puts sub
                end
            end
            io.puts "</table>"
            io.puts "</div>"
            io.string
        end
    end

    def stub
        StringIO.open do |io|
            io.puts "<em>Dieser Artikel ist noch nicht vollständig. Schreib eine E-Mail an <a href='mailto:specht@gymnasiumsteglitz.de'>specht@gymnasiumsteglitz.de</a>, wenn du dabei helfen möchtest, ihn fertigzustellen.</em>"
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
            email = row['u.email']
            next if @@teachers.include?(email)
            lines << { :email => email, :code => row['l.code'] }
        end
        @@clients.each_pair do |client_id, ws|
            filtered_lines = lines.select do |line|
                ws_email = @@email_for_client_id[client_id]
                email = line[:email]
                group = @@invitations[email][:group]
                ADMIN_USERS.include?(ws_email) ||(@@teachers[ws_email] || Set.new()).include?(group)
            end
            ws.send({:action => 'login_codes', :lines => filtered_lines}.to_json)
        end
    end

    get '/ws' do
        assert(teacher_logged_in?)
        if Faye::WebSocket.websocket?(request.env)
            ws = Faye::WebSocket.new(request.env)

            ws.on(:open) do |event|
                client_id = request.env['HTTP_SEC_WEBSOCKET_KEY']
                ws.send({:hello => 'world'})
                @@clients[client_id] = ws
                @@email_for_client_id[client_id] = @session_user[:email]
                @@client_ids_for_email[@session_user[:email]] ||= []
                @@client_ids_for_email[@session_user[:email]] << client_id
                email_for_tag = {}
                neo4j_query(<<~END_OF_STRING).each do |row|
                    MATCH (u:User) RETURN u.email;
                END_OF_STRING
                    email = row['u.email']
                    email_for_tag[fs_tag_for_email(email)] = email
                end
                @@threads_for_client_id[client_id] ||= {}
                @@threads_for_client_id[client_id][:docker_stats] ||= Thread.new do
                    command = "docker stats --format \"{{ json . }}\""
                    lines = {}
                    count = 0
                    IO.popen(command).each_line do |line|
                        if line[0].ord == 0x1b
                            count = (count + 1) % 2
                            ws.send({:stats => lines}.to_json)
                            lines = {}
                            line = line[line.index('{'), line.size]
                        end
                        if line.include?(0x1b.chr)
                            line = line[0, line.index(0x1b.chr)]
                        end
                        line.strip!
                        next if line.empty?
                        stat_line = JSON.parse(line)
                        name = stat_line['Name']
                        if name[0, 8] == 'hs_code_'
                            fs_tag = name.sub('hs_code_', '')
                            email = email_for_tag[fs_tag]
                            lines[fs_tag] = {
                                :name => (@@invitations[email] || {})[:name],
                                :group => (@@invitations[email] || {})[:group],
                                :stats => stat_line
                            }
                        end
                    end
                end
                @@threads_for_client_id[client_id][:host_stats] ||= Thread.new do
                    loop do
                        data = {}

                        mpstat = `mpstat 1 1 | grep 'Average' |grep 'all'`.strip
                        parts = mpstat.split(/\s+/)
                        cpu_idle = (parts.last.to_f * 100).to_i / 100.0
                        cpu_used = ((100.0 - cpu_idle) * 100).to_i / 100.0
                        data[:cpu_usage] = cpu_used

                        num_cores = `grep -c ^processor /proc/cpuinfo`.to_i
                        data[:num_cores] = num_cores
                        meminfo = `free -k | grep 'Mem:'`

                        parts = meminfo.split(/\s+/)
                        ram_total = parts[1].to_i * 1024
                        ram_available = parts[6].to_i * 1024
                        ram_used = ram_total - ram_available
                        data[:ram_total] = bytes_to_str(ram_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:ram_used] = bytes_to_str(ram_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:ram_percent] = (ram_used * 100.0 / ram_total * 100).to_i.to_f / 100

                        diskinfo = `df -kP /`.split("\n").last.strip
                        parts = diskinfo.split(/\s+/)
                        disk_total = parts[1].to_i * 1024
                        disk_free = parts[3].to_i * 1024
                        disk_used = disk_total - disk_free
                        data[:disk_root_total] = bytes_to_str(disk_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_root_used] = bytes_to_str(disk_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_root_percent] = (disk_used * 100.0 / disk_total * 100).to_i.to_f / 100

                        diskinfo = `df -kP /user`.split("\n").last.strip
                        parts = diskinfo.split(/\s+/)
                        disk_total = parts[1].to_i * 1024
                        disk_free = parts[3].to_i * 1024
                        disk_used = disk_total - disk_free
                        data[:disk_user_total] = bytes_to_str(disk_total).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_user_used] = bytes_to_str(disk_used).sub('B', '').gsub(' ', '').gsub(/([A-Za-z]+)/, '<span class=\'unit\'>\1</span>')
                        data[:disk_user_percent] = (disk_used * 100.0 / disk_total * 100).to_i.to_f / 100
                        
                        ws.send({:server_stats => data}.to_json)

                        sleep 5
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
                    @@threads_for_client_id[client_id].keys.each do |key|
                        @@threads_for_client_id[client_id][key].kill
                    end
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

    # get '/json' do
    #     params = request.params
    #     STDERR.puts params.to_yaml
    #     if params['fn'] == 'version'
    #     elsif params['fn'] == 'dir'
    #         respond(:folders => [{:name => 'games'}], :files => [])
    #     end
    # end

    get '/export/1.1/:tag' do
        tag = params['tag']
        assert(%w(html linux mac rpi win winxp).include?(tag))
        redirect "#{WEB_ROOT}/tic80/export/1.1/#{tag}"
    end

    get '/api/hs_get_all_stored_dirs_and_files' do
        assert(user_logged_in?)
        dirs = []
        files = []
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[r:HAS]->(f:TIC80Dir)
            RETURN f, r;
        END_OF_STRING
            dirs << {
                :path => row['f'][:path],
            }
        end
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[r:HAS]->(f:TIC80File)
            RETURN f, r;
        END_OF_STRING
            files << {
                :path => row['f'][:path],
                :sha1 => row['r'][:sha1],
                :contents => Base64.encode64(File.read("/tic80/#{row['r'][:sha1][0, 2]}/#{row['r'][:sha1][2, row['r'][:sha1].size - 2]}")),
            }
        end
        dirs.sort! do |a, b|
            a[:path] <=> b[:path]
        end
        files.sort! do |a, b|
            a[:path] <=> b[:path]
        end

        respond(:dirs => dirs, :files => files)
    end

    post '/api/fs_write' do
        assert(user_logged_in?)
        max_size = 64 * 1024 * 1024
        data = parse_request_data(:required_keys => [:path, :file], :types => {:path => String, :file => Hash}, :max_body_length => max_size, :max_string_length => max_size, :max_value_lengths => {:entry => max_size})
        data[:path] = data[:path].gsub('//', '/')
        data[:path] = '/com.nesbox.tic/TIC-80/' + data[:path] unless data[:path][0] == '/'
        blob = Base64::decode64(data[:file]['contents'])
        sha1 = Digest::SHA1.hexdigest(blob)[0, 16]
        STDERR.puts "[FS_WRITE] #{data[:path]} / size #{data[:file]['contents'].size} / SHA1 #{sha1}"
        path = "/tic80/#{sha1[0, 2]}/#{sha1[2, sha1.size - 2]}"
        FileUtils.mkpath(File.dirname(path))
        unless File.exist?(path)
            File.open(path, 'w') do |f|
                f.write(blob)
            end
        end
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :path => data[:path], :sha1 => sha1})
            MATCH (u:User {email: $email})
            WITH u
            MERGE (f:TIC80File {path: $path})
            WITH u, f
            MERGE (u)-[r:HAS]->(f)
            SET r.sha1 = $sha1
            RETURN f;
        END_OF_STRING
    end

    post '/api/fs_delfile' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:path], :types => {:path => String})
        data[:path] = data[:path].gsub('//', '/')
        data[:path] = '/com.nesbox.tic/TIC-80/' + data[:path] unless data[:path][0] == '/'
        STDERR.puts "[FS_DELFILE] #{data[:path]}"
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :path => data[:path]})
            MATCH (u:User {email: $email})-[r:HAS]->(f:TIC80File {path: $path})
            DELETE r;
        END_OF_STRING
    end

    post '/api/fs_makedir' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:path], :types => {:path => String})
        data[:path] = data[:path].gsub('//', '/')
        data[:path] = '/com.nesbox.tic/TIC-80/' + data[:path] unless data[:path][0] == '/'
        STDERR.puts "[FS_MAKEDIR] #{data[:path]}"
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :path => data[:path]})
            MATCH (u:User {email: $email})
            WITH u
            MERGE (f:TIC80Dir {path: $path})
            WITH u, f
            MERGE (u)-[r:HAS]->(f)
            RETURN f;
        END_OF_STRING
    end

    post '/api/fs_deldir' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:path], :types => {:path => String})
        data[:path] = data[:path].gsub('//', '/')
        data[:path] = '/com.nesbox.tic/TIC-80/' + data[:path] unless data[:path][0] == '/'
        STDERR.puts "[FS_DELDIR] #{data[:path]}"
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :path => data[:path]})
            MATCH (u:User {email: $email})-[r:HAS]->(f:TIC80Dir {path: $path})
            DELETE r;
        END_OF_STRING
    end

    get '/api/ping' do
        respond(:pong => 'yeah')
    end

    post '/api/convert_keras_modal_to_tensorflowjs' do
        file = params['file']
        filename = file['filename']
        assert(!filename.include?('/'))
        assert(filename.size > 0)
        blob = file['tempfile'].read
        sha1 = Digest::SHA1.hexdigest(blob)[0, 16]
        STDERR.puts "Read #{blob.size} bytes with SHA1 #{sha1}"
        path = "/internal/tfjs/#{sha1}"
        FileUtils.mkpath(File.dirname(path))
        File.open(path, 'w') do |f|
            f.write(blob)
        end
        out_path = File.join(File.dirname(path), "#{sha1}-out")
        FileUtils.mkpath(out_path)
        system("docker exec workspace_tensorflowjs_1 tensorflowjs_converter --input_format=keras \"#{path}\" \"#{out_path}\"")
        files = []
        Dir[File.join(out_path, '*')].each do |path|
            files << { :name => File.basename(path), :contents => Base64::encode64(File.read(path)) }
        end
        # FileUtils.rm(path)
        # FileUtils.rm_rf(out_path)
        respond(:uploaded => 'yeah', :files => files)
    end

    post '/api/toggle_show_module' do
        assert(user_logged_in?)

        params = parse_request_data(:required_keys => [:module])
        key = params[:module].to_sym
        assert(MODULE_ORDER.include?(key))

        flag = @session_user["show_#{key}".to_sym] == true
        new_flag = !flag

        email = @session_user[:email]
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :value => new_flag})
            MATCH (u:User {email: $email})
            SET u.show_#{key} = $value
            RETURN u;
        END_OF_STRING

        respond(:yay => 'sure')
    end

    def print_module_button(key)
        assert(user_logged_in?)
        active = @session_user["show_#{key}".to_sym]
        "<button class='btn btn-md #{active ? 'btn-success' : 'btn-outline-secondary'} bu-toggle-module' data-module='#{key}'><i class='bi #{active ? 'bi-check-lg' : 'bi-x-lg'}'></i>#{MODULE_LABELS[key]} im Menü #{active ? '' : 'nicht'} anzeigen</button>"
    end

    post '/api/set_brightness/:mode' do
        mode = params['mode']
        assert(%w(light dark auto).include?(mode))
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :mode => mode})
            MATCH (u:User {email: $email})
            SET u.brightness = $mode
            RETURN u;
        END_OF_STRING
    end

    get '/pdf..*' do
        # Here's a hack to make live reload work with LaTeX PDFs
        # The LaTeX Workshop extensions recompiles the PDF on save
        # and then requests a funny URL to reload the PDF in the browser.
        # This route serves the main.pdf by looking for the latest
        # PDF file in the current user's workspace tree.

        # If the user uses a share tag, we need to look up the email
        # Otherwise, use the email of the logged in user

        referer_path = request.env['HTTP_REFERER']
        referer_path = URI.parse(referer_path).path
        share_tag = referer_path.split('/')[1]
        rows = neo4j_query(<<~END_OF_STRING, {:share_tag => share_tag})
            MATCH (u:User {share_tag: $share_tag})
            RETURN u.email;
        END_OF_STRING
        email = (rows.first || {})['u.email']
        email ||= (@session_user || {})[:email]
        assert(!(email.nil?))
        fs_tag = fs_tag_for_email(email)
        candidates = Dir["/user/#{fs_tag}/workspace/**/*.pdf"]
        candidates.sort! do |a, b|
            File.mtime(b) <=> File.mtime(a)
        end
        respond_with_file(candidates.first)
    end

    post '/api/upload_test_archive' do
        assert(teacher_logged_in?)
        entry = params['file']
        filename = entry['filename']
        blob = entry['tempfile'].read
        sha1 = Digest::SHA1.hexdigest(blob)
        debug "Got a file called #{filename} with #{blob.size} bytes."
        FileUtils.mkpath('/internal/test_archives')
        File.open("/internal/test_archives/#{sha1}", 'w') do |f|
            f.write blob
        end
        ts = Time.now.to_i
        tag = RandomTag.generate(24)
        neo4j_query_expect_one(<<~END_OF_STRING, {:tag => tag, :email => @session_user[:email], :sha1 => sha1, :size => blob.size, :filename => filename, :ts => ts})
            MATCH (u:User {email: $email})
            MERGE (f:File {sha1: $sha1})
            WITH u, f
            CREATE (u)<-[:BELONGS_TO]-(t:Test {tag: $tag})-[:USES]->(f)
            SET f.size = $size
            SET t.filename = $filename
            SET t.ts = $ts
            RETURN f.sha1;
        END_OF_STRING
        respond(:yay => 'sure')
    end

    post '/api/get_my_test_archives' do
        assert(teacher_logged_in?)
        entries = []
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})<-[:BELONGS_TO]->(t:Test)-[:USES]->(f:File)
            OPTIONAL MATCH (t)<-[:TAKES]-(u2:User)
            RETURN t, f, COUNT(u2);
        END_OF_STRING
            entries << {
                tag: row['t'][:tag],
                running: row['t'][:running],
                size: bytes_to_str(row['f'][:size]),
                filename: row['t'][:filename],
                ts: row['t'][:ts],
                count: row['COUNT(u2)'],
            }
        end
        respond(:entries => entries)
    end

    post '/api/delete_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]})
            MATCH (u:User {email: $email})<-[:BELONGS_TO]->(t:Test {tag: $tag})
            DETACH DELETE t;
        END_OF_STRING
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end

    post '/api/get_assigned_users_for_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        emails = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]}).map { |x| x['u.email'] }
            MATCH (u:User)-[:TAKES]->(t:Test {tag: $tag})
            RETURN u.email;
        END_OF_STRING
        respond(:emails => emails)
    end

    post '/api/test_toggle_user' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :email])
        count = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]}).size
            MATCH (u:User {email: $email})-[:TAKES]->(t:Test {tag: $tag})
            RETURN u.email;
        END_OF_STRING
        if count == 0
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MERGE (u:User {email: $email})
                WITH u
                MATCH (t:Test {tag: $tag})
                CREATE (u)-[:TAKES]->(t)
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => true)
        else
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MATCH (u:User {email: $email})-[r:TAKES]->(t:Test {tag: $tag})
                DELETE r
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => false)
        end
    end

    post '/api/start_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            SET t.running = TRUE;
        END_OF_STRING
        Main.refresh_nginx_config()
    end

    post '/api/stop_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            REMOVE t.running;
        END_OF_STRING
        Main.refresh_nginx_config()
    end

    def my_user_group_order
        if admin_logged_in?
            @@user_group_order
        else
            []
        end
    end

    def my_user_groups
        if admin_logged_in?
            @@user_groups
        else
            {}
        end
    end

    def my_invitations
        if admin_logged_in?
            @@invitations
        else
            {}
        end
    end

    def my_running_tests
        unless user_logged_in?
            return []
        end
        entries = []
        neo4j_query(<<~END_OF_QUERY, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[:TAKES]->(t:Test {running: TRUE})-[:BELONGS_TO]->(tu:User)
            RETURN t, tu.email;
        END_OF_QUERY
            entries << {
                :tag => row['t'][:tag],
                :teacher => @@invitations[row['tu.email']][:name]
            }
        end
        entries
    end

    get '/api/pdf_for_test/:tag' do
        assert(teacher_logged_in?)
        test_tag = params['tag']
        html = StringIO.open do |io|
            io.puts <<~END_OF_STRING
                <!DOCTYPE html>
                <html>
                <head>
                    <style>
                        body {
                            font-family: 'Latin Modern Mono', monospace;
                            font-size: 12pt;
                        }
                        pre {
                            font-family: 'Latin Modern Mono', monospace;
                            font-size: 12pt;
                            white-space: pre-wrap;
                        }
                        .file {
                            page-break-before: always;
                        }
                    </style>
                </head>

                <body>
            END_OF_STRING

            neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :test_tag => test_tag}).each do |row|
                MATCH (u:User)-[:TAKES]->(t:Test {tag: $test_tag})-[:BELONGS_TO]->(:User {email: $email})
                RETURN u.email;
            END_OF_STRING
                email = row['u.email']
                email_with_test_tag = "#{email}#{test_tag}"
                container_name = fs_tag_for_email(email_with_test_tag)
                Dir["/user/#{container_name}/workspace/*"].each do |path|
                    if ['txt', 'dart'].include?(path.split('.').last)
                        io.puts "<div class='file'>"
                        io.puts "<div style='display: flex; background-color: #ccc; padding: 0.25em 0.25em; font-weight: bold;'>"
                        io.puts "<div>#{File.basename(path)}</div>"
                        io.puts "<div style='flex-grow: 1; text-align: right;'>#{@@invitations[email][:name]} | #{Time.now.strftime("%d.%m.%Y %H:%M Uhr")}</div>"
                        io.puts "</div>"
                        io.puts "<pre>"
                        lines = File.read(path).split("\n")
                        lines.each_with_index do |line, index|
                            io.puts sprintf("<span style='color: #888; border-right: 1px solid #888; padding-right: 1em;'>%4d</span> %s", index + 1, CGI.escapeHTML(line))
                        end
                        io.puts "</pre>"
                        io.puts "</div>"
                    end
                end
            end

            io.puts "</body>"
            io.puts "</html>"

            io.string
        end
        respond_raw_with_mimetype(html, 'text/html')
    end

    # SELECT 
    #     table_name AS `table`,
    #     table_rows AS `rows`,
    #     data_length + index_length AS size
    # FROM 
    #     information_schema.tables
    # WHERE 
    #     table_schema = 'specht';

    # show create table specht.crew;

    post '/api/get_my_mysql_databases' do
        assert(user_logged_in?)
        client = Mysql2::Client.new(
            host: 'mysql',
            username: 'root',
            password: MYSQL_ROOT_PASSWORD,
        )

        result = {}
        databases = []
        databases << @session_user[:email].split('@').first.downcase
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN d.name AS name;
        END_OF_STRING
            databases << row['name']
        end
        databases.each do |database|
            result[:databases] ||= []
            result[:databases] << database
            result[:tables] ||= {}
            result[:tables][database] = {}
            result[:total] ||= {}
            result[:total][database] = {
                :rows => 0,
                :size => 0
            }
            client.query("SELECT table_name AS `table`, table_rows AS `rows`, data_length + index_length AS size FROM information_schema.tables WHERE table_schema = '#{database}';").each do |row|
                result[:tables][database][row['table']] = {
                    :rows => row['rows'],
                    :size => row['size'],
                }
                result[:total][database][:rows] += row['rows']
                result[:total][database][:size] += row['size']
            end
        end
        respond(:result => result)
    end

    post '/api/create_mysql_database' do
        assert(user_logged_in?)
        count = neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email]})['count']
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql'})
            RETURN COUNT(d) AS count;
        END_OF_STRING
        assert(count < 4)
        database_name = "db_#{RandomTag.generate(12)}"
        client = Mysql2::Client.new(
            host: 'mysql',
            username: 'root',
            password: MYSQL_ROOT_PASSWORD,
        )
        login = @session_user[:email].split('@').first.downcase
        client.query("CREATE DATABASE #{database_name};")
        client.query("GRANT ALL ON `#{database_name}`.* TO '#{login}'@'%';")
        client.query("FLUSH PRIVILEGES;")
        neo4j_query_expect_one(<<~END_OF_STRING, {:email => @session_user[:email], :database_name => database_name})
            MATCH (u:User {email: $email})
            CREATE (d:Database {type: 'mysql', name: $database_name})<-[:HAS]-(u)
            RETURN d;
        END_OF_STRING
        respond(:yay => 'sure')
    end

    post '/api/delete_mysql_database' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:database])
        client = Mysql2::Client.new(
            host: 'mysql',
            username: 'root',
            password: MYSQL_ROOT_PASSWORD,
        )
        is_user_db = (data[:database] == @session_user[:email].split('@').first.downcase)
        client.query("DROP DATABASE IF EXISTS `#{data[:database]}`;")
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :database => data[:database]})
            MATCH (u:User {email: $email})-[:HAS]->(d:Database {type: 'mysql', name: $database})
            DETACH DELETE d;
        END_OF_STRING
        if is_user_db
            login = @session_user[:email].split('@').first.downcase
            client.query("CREATE DATABASE `#{login}`;")
            client.query("GRANT ALL ON `#{login}`.* TO '#{login}'@'%';")
            client.query("FLUSH PRIVILEGES;")
        end
    end

    post '/api/automatron' do
        data  = parse_request_data(:required_keys => [:regex])
        regex = safe_regex(data[:regex])

        # ε-NFA
        nfa = NFA.from_regex(regex)
        nfa.compact_ids!
        nfa.topo_relabel!

        # DFA (with subset trace)
        dfa, subset_trace = SubsetTrace.build_with_trace(nfa)

        # Minimized DFA (with table method trace)
        min, min_trace = DFAMinimizer.minimize_with_trace(dfa)

        # DOT
        nfa_dot           = DotRender.nfa(nfa)
        dfa_dot_explicit  = DotRender.dfa(dfa, title: 'DFA',                 sink_explicit: true,  q: 'r')
        dfa_dot_implicit  = DotRender.dfa(dfa, title: 'DFA_ImplicitSink',    sink_explicit: false, q: 'r')
        min_dot_explicit  = DotRender.dfa(min, title: 'MinDFA',              sink_explicit: true,  q: 's')
        min_dot_implicit  = DotRender.dfa(min, title: 'MinDFA_ImplicitSink', sink_explicit: false, q: 's')

        # SVG
        nfa_svg           = svg_from_dot(nfa_dot)
        dfa_svg           = svg_from_dot(dfa_dot_explicit)
        dfa_implicit_svg  = svg_from_dot(dfa_dot_implicit)
        min_svg           = svg_from_dot(min_dot_explicit)
        min_implicit_svg  = svg_from_dot(min_dot_implicit)

        # Grammar (build structure + pretty text)
        gram_struct = RegularGrammar.build(min)
        # Classic (step-by-step) grammar text (hides nonproductive targets)
        grammar     = RegularGrammar.to_html(gram_struct, hide_nonproductive: true)
        # Simplified / compact grammar (merging linear chains, e.g., S → ab)
        gram_simpl   = RegularGrammar.simplify(gram_struct)
        grammar_compact = RegularGrammar.to_html_compact(gram_simpl)

        # Random samples (positives from grammar, negatives from DFA)
        samples_mixed = GrammarSampler.sample_mixed(
            min, gram_struct,
            pos: 30,      # how many positives
            neg: 30,      # how many negatives
            max_steps: 30,
            eps_bias: 1.0,
            max_len: 20,
            stop_prob: 0.25,
            seed: Time.now.to_i      # remove/alter for fresh sets each call
        )

        # Unwrapped HTML snippets (each is a <section>…</section>)
        subset_html        = HtmlNotes.subset(subset_trace)
        minimization_html  = HtmlNotes.table_minimization(min_trace)

        payload = {
            input: { regex: regex },

            nfa: { dot: nfa_dot, svg: nfa_svg },

            dfa: {
                explicit_sink: { dot: dfa_dot_explicit, svg: dfa_svg },
                implicit_sink: { dot: dfa_dot_implicit, svg: dfa_implicit_svg, has_sink: !!DotRender.detect_sink(dfa) }
            },

            min_dfa: {
                explicit_sink: { dot: min_dot_explicit, svg: min_svg },
                implicit_sink: { dot: min_dot_implicit, svg: min_implicit_svg, has_sink: !!DotRender.detect_sink(min) }
            },

            grammar: grammar,
            grammar_compact: grammar_compact,
            samples_positive: samples_mixed[:positives],
            samples_negative: samples_mixed[:negatives],

            # Two separate, unwrapped sections for easy inclusion
            notes_subset_html:       subset_html,
            notes_minimization_html: minimization_html,

            # If you still want raw trace data for custom UIs, you can add:
            traces: { subset: subset_trace, minimization: min_trace }
        }

        respond(payload)
    end


    get '/*' do
        path = request.path
        slug = nil
        if DEVELOPMENT && path =~ /^\/[a-zA-Z0-9_-]+$/
            Main.parse_content()
        end
        running_tests = my_running_tests()
        if path[0, 7] == '/share/'
            share_tag = path.sub('/share/', '')
            STDERR.puts "OPENING SHARE WITH SHARE TAG #{share_tag}"
            begin
                share_user = neo4j_query_expect_one(<<~END_OF_STRING, :share_tag => share_tag)['u']
                    MATCH (u:User {share_tag: $share_tag})
                    RETURN u;
                END_OF_STRING
            rescue
                redirect "#{WEB_ROOT}/", 302
                return
            end
            path = '/share.html'
        end
        if @@content.include?(path[1, path.size - 1])
            slug = path[1, path.size - 1]
            path = '/a.html'
        end
        if path == '/'
            path = '/index.html'
        end
        if path == '/tic80'
            redirect "#{WEB_ROOT}/tic80/", 302
            return
        end
        if path == '/admin' && teacher_logged_in?
            Main.load_invitations()
        end
        if path == '/tic80/'
            s = File.read(File.join(@@static_dir, '/tic80/index.html'))
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
            respond_raw_with_mimetype(s, 'text/html')
            return
        end
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
                response.set_cookie('hs_sid',
                    :domain => ".#{WEBSITE_HOST}",
                    :value => sid,
                    :expires => expires,
                    :path => '/',
                    :httponly => true,
                    :secure => DEVELOPMENT ? false : true)
                response.set_cookie('hs_server_sid',
                    :domain => ".#{WEBSITE_HOST}",
                    :value => server_sid_for_email(email),
                    :expires => expires,
                    :path => "/", #"/#{fs_tag_for_email(email)}",
                    :httponly => true,
                    :secure => DEVELOPMENT ? false : true)
                Thread.new do
                    postgres_password = Main.gen_password_for_email(email, POSTGRES_PASSWORD_SALT)
                    system("docker exec -i workspace_pgadmin_1 /venv/bin/python setup.py add-user #{email} #{postgres_password}")
                end
            rescue
            end
            redirect "#{WEB_ROOT}/", 302
        end
        if path[0, 7] == '/logout'
            path = '/index.html'
            neo4j_query(<<~END_OF_QUERY, {:sid => request.cookies['hs_sid']})
                MATCH (s:Session {sid: $sid})
                DETACH DELETE s;
            END_OF_QUERY
            response.set_cookie('hs_sid',
                :domain => ".#{WEBSITE_HOST}",
                :value => nil,
                :expires => Time.new + 3600 * 24 * 365,
                :path => '/',
                :httponly => true,
                :secure => DEVELOPMENT ? false : true)
            redirect "#{WEB_ROOT}/", 302
        end
        path = path + '.html' unless path.include?('.')
        path = File.join(@@static_dir, path)
        assert(File.absolute_path(path).start_with?(@@static_dir))
        respond_with_file(path) do |content, mime_type|
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
                    if code[0] != '<'
                        begin
                            s[index, length] = eval(code).to_s || ''
                        rescue
                            STDERR.puts "Error while evaluating:"
                            STDERR.puts code
                            raise
                        end
                    else
                        s.sub!('#{', '&#35;{')
                    end
                end
                s
            end
        end
    end
end
