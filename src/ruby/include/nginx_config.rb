# The nginx configuration the site writes for itself: a server block for
# every running Workspace and Live App, the cookie maps that keep the site's
# own cookies away from student code, the client keys, and the reload with
# HUP. Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    @@nginx_config_mutex = Mutex.new

    # nginx maps that remove hs_server_sid and hs_watch_tag from a Cookie
    # header, one per step: $http_cookie -> $hs_cookie_for_student.
    HS_COOKIE_STEPS = 4

    def self.nginx_cookie_maps
        (1..HS_COOKIE_STEPS).map do |i|
            from = i == 1 ? '$http_cookie' : "$hs_cookie_#{i - 1}"
            to = i == HS_COOKIE_STEPS ? '$hs_cookie_for_student' : "$hs_cookie_#{i}"
            "map #{from} #{to} {\n" \
                "                default #{from};\n" \
                "                \"~^(?<hs_ck_a#{i}>(?:.*?;\\s*)?)(?:hs_server_sid|hs_watch_tag)=[^;]*(?:;\\s*)?(?<hs_ck_b#{i}>.*)$\" " \
                "\"$hs_ck_a#{i}$hs_ck_b#{i}\";\n" \
                "            }"
        end.join("\n\n            ")
    end

    # The subnets of the user network (where the Workspaces are), from the
    # networks of the nginx container whose name ends in _user; kept once
    # found (not while nginx is not up yet). Here, before the nginx config,
    # whose first one is written while this class still loads.
    @@user_subnets = nil

    def self.user_subnets
        return @@user_subnets if @@user_subnets
        found = begin
            names, = Open3.capture2('docker', 'ps', '--filter', 'label=com.docker.compose.service=nginx', '--format', '{{.Names}}')
            networks = names.split.flat_map do |name|
                out, = Open3.capture2('docker', 'inspect', '-f', '{{range $n, $c := .NetworkSettings.Networks}}{{$n}} {{end}}', name)
                out.split.select { |n| n.end_with?('_user') }
            end
            networks.uniq.flat_map do |network|
                out, = Open3.capture2('docker', 'network', 'inspect', '-f', '{{range .IPAM.Config}}{{.Subnet}}={{.Gateway}} {{end}}', network)
                out.split.filter_map do |entry|
                    cidr, gateway = entry.split('=')
                    { :net => IPAddr.new(cidr), :gateway => gateway.to_s } rescue nil
                end
            end
        end
        @@user_subnets = found unless found.empty?
        found
    rescue => e
        STDERR.puts "user subnets: #{e.class}: #{e.message}"
        []
    end

    # Limits per browser in nginx, against floods from a few addresses (a
    # flood from many is Hetzner's to filter): requests to the Ruby app per
    # second with a burst, and connections open at once (long WebSockets
    # included) everywhere. The burst is generous, since a school's classes
    # reach the server from one address: a class opening a page at once makes
    # a few hundred requests, each Workspace keeps a few WebSockets open. The
    # rate stays well below what the app serves: one Puma process renders
    # with one core (about 14 pages a second in development), so that one
    # address alone cannot keep it busy.
    NGINX_BACKEND_RATE = 10
    NGINX_BACKEND_BURST = 300
    NGINX_CONNECTIONS_PER_CLIENT = 1000

    # Which address a request counts for: through Traefik the last entry of
    # X-Forwarded-For (as client_ip), without it the peer. A Workspace
    # reaches nginx directly on the user network and could write any header,
    # so there the peer counts (its gateway is the host, as in
    # in_user_network?).
    # In development a request without the header (the browser, Playwright,
    # the screenshot service, all from the host) has no key, and nginx
    # counts nothing without one; with the header it is limited as behind
    # Traefik, which is how the limits are tried there.
    def self.nginx_client_key_config
        subnets = user_subnets.map do |s|
            "    #{s[:net]}/#{s[:net].prefix} 1;" + (s[:gateway].empty? ? '' : "\n    #{s[:gateway]}/32 0;")
        end
        <<~END_OF_STRING
            geo $hs_from_workspace {
                default 0;
            #{subnets.join("\n")}
            }
            map $http_x_forwarded_for $hs_forwarded_last {
                default "";
                "~(?:^|,)\\s*(?<a>[^,\\s]+)\\s*$" $a;
            }
            map "$hs_from_workspace:$hs_forwarded_last" $hs_client {
                default $hs_forwarded_last;
                "~^1:" $remote_addr;
                "0:" #{DEVELOPMENT ? '""' : '$remote_addr'};
            }
            limit_req_zone $hs_client zone=hs_backend:20m rate=#{NGINX_BACKEND_RATE}r/s;
            limit_conn_zone $hs_client zone=hs_conn:20m;
            limit_req_status 429;
            limit_conn_status 429;
            limit_conn hs_conn #{NGINX_CONNECTIONS_PER_CLIENT};
        END_OF_STRING
    end

    def self.refresh_nginx_config
        @@nginx_config_mutex.synchronize do
            refresh_nginx_config_locked
        end
    end

    def self.refresh_nginx_config_locked
        STDERR.puts ">>> Refreshing nginx config..."
        running_servers = {}
        workspace_runtime.running_workspaces(
            :timeout => shell_timeout(:docker_network_inspect),
        ).each_pair do |fs_tag, workspace|
            running_servers[fs_tag] = {
                :ip => workspace[:ip],
                :server_sid => nil,
                :server_tag => nil,
                :share_tag => nil,
                :watch_tags => {}, # watch_tag => server_id of watcher
                :live_apps => {},  # live share tag => port
            }
        end

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

        users.each do |row|
            user = row['u']
            test = row['t']
            email_with_test_tag = "#{user[:email]}#{(test || {})[:tag]}"
            server_tag = "#{user[:server_tag]}#{(test || {})[:tag]}"
            fs_tag = fs_tag_for_email(email_with_test_tag)
            next unless may_use_workspace?(user[:email].to_s, user)
            if running_servers.include?(fs_tag)
                # sid_ip_pairs << "#{user[:server_sid]} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:server_sid] = user[:server_sid]
                running_servers[fs_tag][:server_tag] = server_tag
                running_servers[fs_tag][:share_tag] = user[:share_tag] if user[:share_expires].nil? || user[:share_expires].to_i > Time.now.to_i
            end
        end

        # Teacher test demos use the same DNS-disabled test runtime as students,
        # but live under a separate private workspace tag and are never shared.
        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User)<-[:BELONGS_TO]-(t:Test)
            RETURN u, t;
        END_OF_QUERY
            user = row['u']
            test = row['t']
            demo_tag = test_demo_tag(user[:email], test[:tag])
            fs_tag = fs_tag_for_email("#{user[:email]}#{demo_tag}")
            next unless running_servers.include?(fs_tag)
            # Only while its teacher still is one, with its own access
            next unless may_use_workspace?(user[:email].to_s, user) &&
                        (ADMIN_USERS.include?(user[:email].to_s) || @@teachers.include?(user[:email].to_s))

            running_servers[fs_tag][:server_sid] = user[:server_sid]
            running_servers[fs_tag][:server_tag] = "#{user[:server_tag]}#{demo_tag}"
        end
        # STDERR.puts "sid_ip_pairs:"
        # STDERR.puts sid_ip_pairs.join("\n")

        # watch_tag_ip_pairs = []

        # A watched Workspace only while the watcher still may look into it
        # (its groups and its own access as they are now); a watch it may no
        # longer have is removed.
        stale_watches = []
        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (w:User)-[r:WATCHING]->(u:User)
            RETURN r.watch_tag, u, w;
        END_OF_QUERY
            watch_tag = row['r.watch_tag']
            user = row['u']
            watcher = row['w']
            next unless may_use_workspace?(user[:email].to_s, user)
            unless may_still_look_in?(watcher[:email].to_s, user[:email].to_s, watcher)
                stale_watches << watch_tag
                next
            end
            fs_tag = fs_tag_for_email(user[:email])
            if running_servers.include?(fs_tag)
                # watch_tag_ip_pairs << "#{watch_tag} #{running_servers[fs_tag][:ip]}:8443;"
                running_servers[fs_tag][:watch_tags][watch_tag] = watcher[:server_sid]
            end
        end
        unless stale_watches.empty?
            $neo4j.neo4j_query('MATCH (:User)-[r:WATCHING]->(:User) WHERE r.watch_tag IN $tags DELETE r;', :tags => stale_watches)
            STDERR.puts ">>> Removed #{stale_watches.size} watch(es) no longer allowed"
        end

        $neo4j.neo4j_query(<<~END_OF_QUERY).each do |row|
            MATCH (u:User)-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
            RETURN u.email, s.port, s.tag, u;
        END_OF_QUERY
            next unless may_use_workspace?(row['u.email'].to_s, row['u'])
            fs_tag = fs_tag_for_email(row['u.email'])
            next unless running_servers.include?(fs_tag)
            running_servers[fs_tag][:live_apps][row['s.tag']] = row['s.port'].to_i
        end

        # STDERR.puts ">>> Got running servers: #{running_servers.to_yaml}"

        # STDERR.puts "watch_tag_ip_pairs:"
        # STDERR.puts watch_tag_ip_pairs.join("\n")

        nginx_config = <<~END_OF_STRING
            #log_format custom '$host $http_x_forwarded_proto $remote_addr - $remote_user [$time_local] "$request" '
            #                '$status $body_bytes_sent "$http_referer" '
            #                '"$http_user_agent" "$request_time"';

            map_hash_bucket_size 128;

            # Limits per browser (NGINX_BACKEND_RATE and the rest)
            #{nginx_client_key_config.rstrip}

            # Keep access logs useful without recording capability tokens, session
            # cookies, workspace tags or one-time login URLs.
            map $uri $hs_log_uri {
                default $uri;
                ~^/w/[a-z0-9]+(?:/|$) /w/[redacted];
                ~^/share/[a-z0-9]+(?:/|$) /share/[redacted];
                ~^/l/[a-z0-9]+(?:/|$) /l/[redacted];
            }

            log_format custom 'time=$time_iso8601 method=$request_method uri=$hs_log_uri status=$status share=$hs_is_share allowed=$hs_allowed';

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

            map $http_upgrade $connection_upgrade {
                default upgrade;
                ''      close;
            }

            # code-server HTML must arrive uncompressed so sub_filter can inject
            # the webfont stylesheet. Keep gzip for JS/CSS/other assets.
            map $http_accept $hs_code_accept_encoding {
                default gzip;
                ~*text/html "";
            }

            # Inject the browser-visible code font and workspace.css only into
            # the code-server workbench document. Internal HTML such as the web-worker extension
            # host has its own restrictive CSP and must remain untouched.
            map $request_uri $hs_code_font_head {
                default "</head>";
                ~^/(?:w/[a-z0-9]+/)?(?:\\?.*)?$ '<link rel="preload" href="/include/fonts/0xProtoNerdFontMono/0xProtoNerdFontMono-Regular.woff2" as="font" type="font/woff2" crossorigin><link rel="stylesheet" href="/include/fonts.css?#{CACHE_BUSTER}"><link rel="stylesheet" href="/include/workspace.css?#{CACHE_BUSTER}"></head>';
            }

            # ---------------------------
            # Workspace token extraction
            # ---------------------------

            # Extract token from /w/<token>/...
            map $uri $w_token {
                default "";
                ~^/w/(?<t>[a-z0-9]+)(?:/|$) $t;
            }

            # Extract token from <token>.#{WEBSITE_HOST.split(':').first}
            map $host $host_token {
                default "";
                ~^(?<t>[a-z0-9]+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')} $t;
            }

            # Effective token: prefer subdomain token, else /w/ token
            map "$host_token:$w_token" $hs_token {
                default $w_token;
                ~^(?<t>[^:]+): $t;
            }

            # Token -> upstream
            map $hs_token $hs_upstream {
                default "";
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then "#{info[:server_tag]} http://#{info[:ip]}:8443;" else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then info[:watch_tags].map { |watch_tag, server_sid| "#{watch_tag} http://#{info[:ip]}:8443;" }.flatten.compact.join("\n    ") else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:share_tag] then "#{info[:share_tag]} http://#{info[:ip]}:8443;" else nil end}.compact.join("\n    ")}
            }

            # Token -> share flag (explicit allowlist)
            map $hs_token $hs_is_share {
                default 0;
                #{running_servers.map { |fs_tag, info| if info[:share_tag] then "#{info[:share_tag]} 1;" else nil end}.compact.join("\n    ")}
            }

            # Token -> required SID
            map $hs_token $hs_required_sid {
                default "";
                #{running_servers.map { |fs_tag, info| if info[:server_sid] then "#{info[:server_tag]} #{info[:server_sid]};" else nil end}.compact.join("\n    ")}
                #{running_servers.map { |fs_tag, info| if info[:server_tag] then info[:watch_tags].map { |watch_tag, server_sid | "#{watch_tag} #{server_sid};" }.flatten.compact.join("\n    ") else nil end}.compact.join("\n    ")}
            }

            # Shared live apps: stable tag -> workspace proxy + port
            map $host $hs_live_tag {
                default "";
                ~^live-(?<lt>[a-z0-9]+)\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$ $lt;
            }

            map $hs_live_tag $hs_live_upstream {
                default "";
                #{running_servers.map { |_fs_tag, info| info[:live_apps].map { |tag, _port| "#{tag} http://#{info[:ip]}:8443;" } }.flatten.join("\n    ")}
            }

            map $hs_live_tag $hs_live_port {
                default "";
                #{running_servers.map { |_fs_tag, info| info[:live_apps].map { |tag, port| "#{tag} #{port};" } }.flatten.join("\n    ")}
            }

            map $hs_live_upstream $hs_live_no_upstream {
                ""      1;
                default 0;
            }

            # The Cookie header for a student's container, without the
            # domain-wide hs_server_sid and hs_watch_tag: nginx checks them
            # above, and they must never reach code a student controls
            # (code-server, apps on <tag>-<port>). The student's own cookies
            # stay. Each step removes one; a browser sends each of them at
            # most once per domain and path, so four steps leave none.
            #{Main.nginx_cookie_maps}

            # deny if upstream missing
            map $hs_upstream $hs_no_upstream {
                ""      1;
                default 0;
            }

            # deny if token is private but required SID missing (fail closed)
            map "$hs_is_share:$hs_required_sid" $hs_private_missing_sid {
                default 0;
                "0:"    1;
            }

            # Allowed if:
            # - share token, OR
            # - cookie SID matches required SID
            map "$hs_is_share:$cookie_hs_server_sid:$hs_required_sid" $hs_allowed {
                default 0;

                # Share token => allow regardless of cookie
                ~^1: 1;

                # Private token => cookie must equal required sid
                ~^0:(?<sid>[^:]+):\\k<sid>$ 1;
            }

            server {
                listen 80;
                server_name #{WEBSITE_HOST.split(':').first};
                client_max_body_size 100M;
                expires $expires;
                add_header Permissions-Policy "local-fonts=()" always;

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

                location = /cache { return 301 /cache/; }
                location ^~ /cache/ { alias /webcache/; }

                location = /brand { return 301 /brand/; }
                location ^~ /brand/ { alias /brand/; }

                location = /dl { return 301 /dl/; }
                location ^~ /dl/ { alias /dl/; }

                location / {
                    root /usr/share/nginx/html;
                    try_files $uri @ruby;
                }

                # Python and Ruby for the turtle (fetch-turtle-runtimes.sh):
                # gzipped beforehand, kept by the browser, as the version
                # is in the path.
                location ^~ /include/turtle/runtime/ {
                    root /usr/share/nginx/html;
                    gzip_static on;
                    expires max;
                    types {
                        application/wasm wasm;
                        text/javascript js mjs;
                        application/json json;
                        application/zip zip;
                    }
                    try_files $uri =404;
                }

                # nginx knows no type for .webmanifest.
                location = /site.webmanifest {
                    root /usr/share/nginx/html;
                    types { }
                    default_type application/manifest+json;
                }

                location @ruby {
                    limit_req zone=hs_backend burst=#{NGINX_BACKEND_BURST} nodelay;
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://ruby:9292;
                }

                # Normalize /w/<token> -> /w/<token>/
                location ~ ^/w/[a-z0-9]+$ {
                    return 301 $uri/$is_args$args;
                }

                location ~ ^/w/[a-z0-9]+/ {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    # strip /w/<token>
                    rewrite ^/w/[a-z0-9]+(.*)$ $1 break;

                    include /etc/nginx/snippets/proxy_ws_code.conf;
                    proxy_set_header Cookie $hs_cookie_for_student;
                    sub_filter_once on;
                    sub_filter '</head>' $hs_code_font_head;
                    proxy_pass $hs_upstream;
                }
            }

            # ==================================
            # Subdomains: <token>.#{WEBSITE_HOST.split(':').first}
            # ==================================
            server {
                listen 80;
                server_name ~^(?<t>[a-z0-9]+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;
                add_header Permissions-Policy "local-fonts=()" always;

                # ----------------------------------
                # /proxy/<port> -> <token>-<port>.#{WEBSITE_HOST.split(':').first}
                # ----------------------------------
                location ~ ^/proxy/(?<p>\\d+)(?<rest>/.*)?$ {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    # Default rest to /
                    set $r $rest;
                    if ($r = "") { set $r /; }

                    # pick up external port from Host header (e.g. :8025)
                    set $port "";
                    if ($http_host ~* :(?<hp>\\d+)$) { set $port :$hp; }

                    return 302 #{DEVELOPMENT ? 'http:' : 'https:'}//${t}-${p}.#{WEBSITE_HOST.split(':').first}$port$r$is_args$args;
                }

                # Browser-visible Hackschule fonts for the code-server workbench.
                # These are public static assets; workspace authentication remains
                # on the code-server proxy itself.
                location = /include/fonts.css {
                    root /usr/share/nginx/html;
                    try_files $uri =404;
                }

                # Hackschule's own styles for the workbench (terminal rows).
                location = /include/workspace.css {
                    root /usr/share/nginx/html;
                    try_files $uri =404;
                }

                location ^~ /include/fonts/0xProtoNerdFontMono/ {
                    root /usr/share/nginx/html;
                    try_files $uri =404;
                }

                location / {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    include /etc/nginx/snippets/proxy_ws_code.conf;
                    proxy_set_header Cookie $hs_cookie_for_student;
                    sub_filter_once on;
                    sub_filter '</head>' $hs_code_font_head;
                    proxy_pass $hs_upstream;
                }
            }

            # ==========================================
            # Port subdomains: <tag>-<port>.#{WEBSITE_HOST.split(':').first}
            # ==========================================
            server {
                listen 80;
                server_name ~^(?<t>[a-z0-9]+)-(?<p>\\d+)\\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;
                add_header Permissions-Policy "local-fonts=()" always;

                set $hs_token $t;

                location / {
                    if ($hs_no_upstream) { return 404; }
                    if ($hs_private_missing_sid) { return 403; }
                    if ($hs_allowed = 0) { return 403; }

                    rewrite ^/(.*)$ /proxy/$p/$1 break;

                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_set_header Cookie $hs_cookie_for_student;

                    # First remove leaked /proxy/<port>/ redirects.
                    proxy_redirect ~^/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+/?$ /;

                    # Any remaining absolute redirect must use the browser-visible host.
                    # $http_host includes :8025 in development.
                    proxy_redirect ~^(https?://)[^/]+(/.*)$ $1$http_host$2;
                    proxy_redirect ~^//[^/]+(/.*)$ //$http_host$1;

                    proxy_pass $hs_upstream;
                }
            }

            # ==========================================
            # Shared live apps: live-<tag>.#{WEBSITE_HOST.split(':').first}
            # ==========================================
            server {
                listen 80;
                server_name ~^live-[a-z0-9]+\.#{WEBSITE_HOST.split(':').first.gsub('.', '\\.')}$;
                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location = /_hs_live_auth {
                    internal;
                    proxy_pass http://ruby:9292/api/live_app_authorize;
                    proxy_method GET;
                    proxy_pass_request_body off;
                    proxy_set_header Content-Length "";
                    proxy_set_header Host $http_host;
                    proxy_set_header Cookie $http_cookie;
                }

                location / {
                    if ($hs_live_no_upstream) { return 404; }
                    auth_request /_hs_live_auth;

                    rewrite ^/(.*)$ /proxy/$hs_live_port/$1 break;
                    include /etc/nginx/snippets/proxy_ws.conf;

                    # Never expose Hackschule authentication material to student code.
                    proxy_set_header Cookie "";
                    proxy_set_header Authorization "";
                    proxy_hide_header Set-Cookie;

                    proxy_redirect ~^/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^https?://[^/]+/proxy/[0-9]+/?$ /;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+(/.*)$ $1;
                    proxy_redirect ~^//[^/]+/proxy/[0-9]+/?$ /;

                    # Any remaining absolute redirect must use the browser-visible host.
                    # $http_host includes :8025 in development.
                    proxy_redirect ~^(https?://)[^/]+(/.*)$ $1$http_host$2;
                    proxy_redirect ~^//[^/]+(/.*)$ //$http_host$1;

                    proxy_pass $hs_live_upstream;
                }
            }

            # ===========================================
            # Hand-ins in the browser (SubmissionView)
            # ===========================================
            server {
                listen 80;
                server_name #{SUBMISSION_VIEW_HOST};
                client_max_body_size 1M;
                access_log off;
                charset utf-8;

                location /v/ {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://ruby:9292;
                }

                location /anaglyph-viewer/ {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://ruby:9292;
                }

                location / {
                    return 404;
                }
            }

            # ===========
            # phpMyAdmin
            # ===========
            server {
                listen 80;
                server_name phpmyadmin.#{WEBSITE_HOST.split(':').first};

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://phpmyadmin:80;
                }
            }

            # ======
            # Neo4j
            # ======
            server {
                listen 80;
                server_name ~^neo4j-(?<t>[a-z0-9]+)\.#{WEBSITE_HOST.split(':').first.gsub('.', '\.')}$;

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_pass http://neo4j:7474;
                }
            }

            server {
                listen 80;
                server_name bolt.#{WEBSITE_HOST.split(':').first};

                client_max_body_size 100M;
                access_log /var/log/nginx/access.log custom;
                charset utf-8;

                location / {
                    include /etc/nginx/snippets/proxy_ws.conf;
                    proxy_set_header Sec-WebSocket-Protocol $http_sec_websocket_protocol;
                    proxy_buffering off;
                    proxy_read_timeout 1h;
                    proxy_send_timeout 1h;

                    proxy_pass http://neo4j:7687;
                }
            }
        END_OF_STRING

        STDERR.puts ">>> Writing nginx config..."

        # X-Hs-Peer: whom nginx got the request from (Traefik, or a Workspace on
        # the user network calling nginx directly); client_ip uses it.
        AtomicFile.write('/nginx-snippets/proxy_ws.conf', <<~END_OF_STRING)
            proxy_http_version 1.1;
            proxy_set_header Host $http_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection upgrade;
            proxy_set_header Accept-Encoding gzip;
            proxy_set_header X-Hs-Peer $remote_addr;
        END_OF_STRING

        AtomicFile.write('/nginx-snippets/proxy_ws_code.conf', <<~END_OF_STRING)
            proxy_http_version 1.1;
            proxy_set_header Host $http_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection upgrade;
            proxy_set_header Accept-Encoding $hs_code_accept_encoding;
        END_OF_STRING

        AtomicFile.write('/nginx/default.conf', nginx_config)

        STDERR.puts ">>> Sending HUP to nginx to reload nginx config"
        begin
            shell_ok("docker kill -s HUP workspace_nginx_1", :timeout => shell_timeout(:nginx_reload))
        rescue => hup_error
            nginx_state = shell_capture(
                "docker ps -a --filter name=^/workspace_nginx_1$ --format '{{.State}}'",
                :timeout => shell_timeout(:docker_inspect)
            ).strip
            raise hup_error if nginx_state == 'running'
            STDERR.puts ">>> nginx is not running yet (state: #{nginx_state.empty? ? 'absent' : nginx_state}); it will read the published config on startup"
        end
    end

    private_class_method :refresh_nginx_config_locked
end
