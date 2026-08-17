class Main < Sinatra::Base
    LIVE_APP_MIN_PORT = 1024
    LIVE_APP_MAX_PORT = 65535
    LIVE_APP_BLOCKED_PORTS = Set.new([8443])
    LIVE_APP_USER_UID = 1000
    LIVE_APP_PIN_PATH = '/internal/live_app_pins.json'
    @@live_app_ws_mutex = Mutex.new
    @@live_app_ws_clients = {}
    @@live_app_reconcile_mutex = Mutex.new

    def self.live_app_port_allowed?(port)
        port = port.to_i
        port >= LIVE_APP_MIN_PORT && port <= LIVE_APP_MAX_PORT && !LIVE_APP_BLOCKED_PORTS.include?(port)
    end

    def self.live_app_listening_sockets_for_email(email)
        fs_tag = fs_tag_for_email(email)
        sockets = workspace_runtime.live_app_sockets(
            fs_tag,
            :uid => LIVE_APP_USER_UID,
            :timeout => shell_timeout(:docker_inspect),
        ).select do |socket|
            live_app_port_allowed?(socket[:port])
        end

        sockets.uniq { |socket| [socket[:port], socket[:inode]] }.sort_by { |socket| [socket[:port], socket[:inode].to_s] }
    rescue => e
        STDERR.puts ">>> Could not inspect live-app sockets for #{email}: #{e.class}: #{e.message}"
        []
    end

    def self.live_app_open_ports_for_email(email)
        live_app_listening_sockets_for_email(email).map { |socket| socket[:port] }.uniq.sort
    end

    def self.live_app_socket_signature(sockets, port)
        sockets
            .select { |socket| socket[:port] == port.to_i }
            .map { |socket| socket[:inode].to_s }
            .reject(&:empty?)
            .uniq
            .sort
            .join(',')
    end

    def self.live_app_open_port_signature_for_email(email)
        live_app_listening_sockets_for_email(email).map { |socket| [socket[:port], socket[:inode]] }
    end

    def self.live_app_processes_for_email(email)
        fs_tag = fs_tag_for_email(email)
        workspace_runtime.live_app_processes(
            fs_tag,
            :uid => LIVE_APP_USER_UID,
            :timeout => shell_timeout(:docker_inspect),
        )
    rescue => e
        STDERR.puts ">>> Could not inspect live-app processes for #{email}: #{e.class}: #{e.message}"
        {}
    end

    def self.live_app_open_port_details_for_email(email)
        sockets = live_app_listening_sockets_for_email(email)
        return [] if sockets.empty?

        processes = live_app_processes_for_email(email)
        by_port = {}

        sockets.each do |socket|
            process = processes[socket[:inode]] || {}
            candidate = {
                :port => socket[:port],
                :pid => process[:pid],
                :process => process[:process],
                :command => process[:command],
            }
            existing = by_port[socket[:port]]
            if existing.nil? || (existing[:process].to_s.empty? && !candidate[:process].to_s.empty?)
                by_port[socket[:port]] = candidate
            end
        end

        by_port.values.sort_by { |entry| entry[:port] }
    end

    def self.register_live_app_ws(client_id, ws)
        @@live_app_ws_mutex.synchronize do
            @@live_app_ws_clients[client_id] = ws
        end
    end

    def self.unregister_live_app_ws(client_id)
        @@live_app_ws_mutex.synchronize do
            @@live_app_ws_clients.delete(client_id)
        end
    end

    def self.broadcast_live_app_refresh
        clients = @@live_app_ws_mutex.synchronize { @@live_app_ws_clients.values.dup }
        message = {:action => 'refresh_live_apps'}.to_json
        clients.each do |ws|
            begin
                ws.send(message)
            rescue
            end
        end
    end

    def self.active_live_app_rows
        2.times do
            begin
                return $neo4j.neo4j_query(<<~END_OF_QUERY).to_a
                    MATCH (u:User)-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
                    RETURN u.email, s.port, s.tag, s.socket_signature;
                END_OF_QUERY
            rescue
                $neo4j = Neo4jGlobal.new
            end
        end
        []
    end

    def self.write_live_app_pins(rows)
        fs_tags = rows.map { |row| fs_tag_for_email(row['u.email']) }.uniq.sort
        FileUtils.mkdir_p(File.dirname(LIVE_APP_PIN_PATH))
        AtomicFile.write(LIVE_APP_PIN_PATH, {:updated_at => Time.now.to_i, :fs_tags => fs_tags}.to_json)
    rescue => e
        STDERR.puts ">>> Could not write live-app pin file: #{e.class}: #{e.message}"
    end

    def self.reconcile_live_apps!(refresh_nginx: true)
        @@live_app_reconcile_mutex.synchronize do
            reconcile_live_apps_locked!(refresh_nginx: refresh_nginx)
        end
    end

    def self.reconcile_live_apps_locked!(refresh_nginx: true)
        rows = active_live_app_rows
        sockets_by_email = {}
        stale_shares = []

        rows.each do |row|
            email = row['u.email']
            port = row['s.port'].to_i
            sockets_by_email[email] ||= live_app_listening_sockets_for_email(email)

            current_signature = live_app_socket_signature(sockets_by_email[email], port)
            shared_signature = row['s.socket_signature'].to_s

            # A share belongs to this particular listening socket, not merely to
            # the port number. A restarted server gets a new socket inode and
            # therefore has to be explicitly shared again.
            if current_signature.empty? || shared_signature.empty? || current_signature != shared_signature
                stale_shares << {
                    :tag => row['s.tag'],
                    :socket_signature => shared_signature,
                }
            end
        end

        deactivated_tags = stale_shares.filter_map do |share|
            row = $neo4j.neo4j_query(<<~END_OF_QUERY, :tag => share[:tag], :socket_signature => share[:socket_signature], :updated_at => Time.now.to_i).first
                MATCH (s:LiveAppShare {tag: $tag})
                WHERE s.active = TRUE AND COALESCE(s.socket_signature, '') = $socket_signature
                SET s.active = FALSE, s.updated_at = $updated_at
                RETURN COUNT(s) AS count;
            END_OF_QUERY
            share[:tag] if row && row['count'].to_i == 1
        end

        rows.reject! { |row| deactivated_tags.include?(row['s.tag']) }
        write_live_app_pins(rows)

        unless deactivated_tags.empty?
            if refresh_nginx
                STDERR.puts ">>> Deactivated #{deactivated_tags.size} stale live-app share(s)."
                refresh_nginx_config
            end
            broadcast_live_app_refresh
        end

        rows
    rescue => e
        STDERR.puts ">>> Live-app reconciliation failed: #{e.class}: #{e.message}"
        []
    end

    private_class_method :reconcile_live_apps_locked!

    def live_app_url(tag)
        scheme = DEVELOPMENT ? 'http' : 'https'
        host = WEBSITE_HOST
        "#{scheme}://live-#{tag}.#{host}/"
    end

    def print_live_apps
        return '' unless user_logged_in?

        email = @session_user[:email]
        open_port_details = Main.live_app_open_port_details_for_email(email)
        my_shares = {}
        neo4j_query(<<~END_OF_QUERY, :email => email).each do |row|
            MATCH (u:User {email: $email})-[:SHARES_LIVE_APP]->(s:LiveAppShare)
            RETURN s;
        END_OF_QUERY
            share = row['s']
            my_shares[share[:port].to_i] = share
        end

        active_apps = neo4j_query(<<~END_OF_QUERY).map do |row|
            MATCH (u:User)-[:SHARES_LIVE_APP]->(s:LiveAppShare {active: TRUE})
            RETURN u.email, s.port, s.tag;
        END_OF_QUERY
            owner_email = row['u.email']
            next if owner_email == email
            invitation = @@invitations[owner_email] || {}
            {
                :email => owner_email,
                :name => invitation[:name] || owner_email,
                :group => invitation[:group],
                :port => row['s.port'].to_i,
                :tag => row['s.tag'],
            }
        end.compact

        my_group = (@@invitations[email] || {})[:group]
        same_group, other_group = active_apps.partition { |app| app[:group] == my_group }
        sorter = lambda { |app| [app[:name].to_s.downcase, app[:port]] }
        same_group.sort_by!(&sorter)
        other_group.sort_by!(&sorter)

        render_app = lambda do |app|
            name = CGI.escapeHTML(app[:name].to_s)
            url = CGI.escapeHTML(live_app_url(app[:tag]))
            "<a class='list-group-item list-group-item-action d-flex justify-content-between align-items-center' target='_blank' rel='noopener' href='#{url}'><strong>#{name}</strong><span class='d-flex align-items-center gap-2'><span class='badge text-bg-secondary'>Port #{app[:port]}</span><i class='bi bi-box-arrow-up-right text-body-secondary'></i></span></a>"
        end

        StringIO.open do |io|
            io.puts "<section id='live-apps' class='my-4'>"
            io.puts "<h3>Shared Live Apps</h3>"
            io.puts "<p>Hier kannst du laufende Web-Apps mit anderen angemeldeten Workspace-Nutzern teilen.</p>"
            io.puts "<h4>Meine laufenden Web-Apps</h4>"
            if open_port_details.empty?
                io.puts "<p class='text-body-secondary'>In deinem Workspace läuft gerade keine teilbare Web-App.</p>"
            else
                io.puts "<div class='table-responsive mb-4'>"
                io.puts "<table class='table table-sm align-middle mb-0' style='table-layout: fixed; min-width: 720px;'>"
                io.puts "<colgroup><col style='width: 6rem;'><col><col style='width: 8rem;'><col style='width: 18rem;'></colgroup>"
                io.puts "<thead><tr><th>Port</th><th>Befehl</th><th>Status</th><th class='text-end'>Aktionen</th></tr></thead>"
                io.puts "<tbody>"
                open_port_details.each do |entry|
                    port = entry[:port]
                    share = my_shares[port]
                    active = share && share[:active] == true
                    command_label = entry[:command].to_s.strip
                    command_label = entry[:process].to_s.strip if command_label.empty?
                    command_html = if command_label.empty?
                        "<span class='text-body-secondary'>–</span>"
                    else
                        escaped_command = CGI.escapeHTML(command_label)
                        "<div class='text-truncate' title='#{escaped_command}'>#{escaped_command}</div>"
                    end

                    status_html = if active
                        "<span class='text-success text-nowrap'><i class='bi bi-check-circle-fill me-1'></i>Geteilt</span>"
                    else
                        "<span class='text-body-secondary text-nowrap'>Nicht geteilt</span>"
                    end

                    io.puts "<tr>"
                    io.puts "<td>#{port}</td>"
                    io.puts "<td style='min-width: 0; font-family: \"IBM Plex Mono\"; font-size: 90%;'>#{command_html}</td>"
                    io.puts "<td>#{status_html}</td>"
                    io.puts "<td class='text-end text-nowrap'>"
                    if active
                        url = CGI.escapeHTML(live_app_url(share[:tag]))
                        io.puts "<a class='btn btn-sm btn-secondary me-2' target='_blank' rel='noopener' href='#{url}'><i class='bi bi-box-arrow-up-right me-1'></i>Öffnen</a><button class='btn btn-sm btn-danger' onclick=\"liveAppAction('unshare', #{port})\"><i class='bi bi-x-circle me-1'></i>Nicht mehr teilen</button>"
                    else
                        io.puts "<button class='btn btn-sm btn-primary' onclick=\"liveAppAction('share', #{port})\"><i class='bi bi-share me-1'></i>Teilen</button>"
                    end
                    io.puts "</td>"
                    io.puts "</tr>"
                end
                io.puts "</tbody></table></div>"
            end

            io.puts "<h4>Von anderen geteilt</h4>"
            if active_apps.empty?
                io.puts "<p class='text-body-secondary'>Zurzeit hat kein anderer Workspace-Nutzer eine laufende App freigegeben.</p>"
            else
                io.puts "<div class='list-group mb-4'>"
                same_group.each { |app| io.puts render_app.call(app) }
                unless same_group.empty? || other_group.empty?
                    io.puts "<div class='list-group-item py-1 text-center small text-body-secondary bg-body-tertiary'>Andere Gruppen</div>"
                end
                other_group.each { |app| io.puts render_app.call(app) }
                io.puts "</div>"
            end
            io.puts "</section>"
            io.string
        end
    end

    get '/api/live_apps' do
        assert(user_logged_in?)
        respond(:html => print_live_apps())
    end

    get '/api/live_app_authorize' do
        halt(user_logged_in? ? 204 : 401)
    end

    post '/api/live_apps/share' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:port], :types => {:port => Integer})
        port = data[:port]
        assert(Main.live_app_port_allowed?(port), 'invalid_port')

        sockets = Main.live_app_listening_sockets_for_email(@session_user[:email])
        socket_signature = Main.live_app_socket_signature(sockets, port)
        assert(!socket_signature.empty?, 'port_not_open')

        tag = gen_share_tag()
        row = neo4j_query_expect_one(<<~END_OF_QUERY, :email => @session_user[:email], :port => port, :tag => tag, :socket_signature => socket_signature, :updated_at => Time.now.to_i)
            MATCH (u:User {email: $email})
            MERGE (u)-[:SHARES_LIVE_APP]->(s:LiveAppShare {port: $port})
            ON CREATE SET s.tag = $tag, s.created_at = $updated_at
            SET s.active = TRUE, s.socket_signature = $socket_signature, s.updated_at = $updated_at
            RETURN s;
        END_OF_QUERY

        Main.reconcile_live_apps!(:refresh_nginx => false)
        Main.refresh_nginx_config()
        Main.broadcast_live_app_refresh
        share = row['s']
        respond(:yay => 'sure', :port => port, :tag => share[:tag], :url => live_app_url(share[:tag]))
    end

    post '/api/live_apps/unshare' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:port], :types => {:port => Integer})
        port = data[:port]
        assert(Main.live_app_port_allowed?(port), 'invalid_port')

        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :port => port, :updated_at => Time.now.to_i)
            MATCH (u:User {email: $email})-[:SHARES_LIVE_APP]->(s:LiveAppShare {port: $port})
            SET s.active = FALSE, s.updated_at = $updated_at;
        END_OF_QUERY

        Main.reconcile_live_apps!(:refresh_nginx => false)
        Main.refresh_nginx_config()
        Main.broadcast_live_app_refresh
        respond(:yay => 'sure', :port => port)
    end

    get '/ws/live_apps' do
        assert(user_logged_in?)
        halt 400 unless Faye::WebSocket.websocket?(request.env)

        email = @session_user[:email]
        ws = Faye::WebSocket.new(request.env)
        client_id = "#{request.env['HTTP_SEC_WEBSOCKET_KEY']}-#{SecureRandom.hex(4)}"
        port_thread = nil

        ws.on(:open) do |_event|
            Main.register_live_app_ws(client_id, ws)
            ws.send({:action => 'refresh_live_apps'}.to_json)

            port_thread = Thread.new do
                last_signature = Main.live_app_open_port_signature_for_email(email)
                loop do
                    sleep 2
                    signature = Main.live_app_open_port_signature_for_email(email)
                    if signature != last_signature
                        last_signature = signature
                        # A socket disappearing or being replaced may invalidate
                        # an active share. Reconcile immediately while this
                        # profile is open; the 10-second global monitor remains
                        # the fallback when nobody is connected here.
                        Main.reconcile_live_apps!
                        ws.send({:action => 'refresh_live_apps'}.to_json)
                    end
                end
            end
        end

        ws.on(:close) do |_event|
            port_thread&.kill
            Main.unregister_live_app_ws(client_id)
        end

        ws.on(:error) do |_event|
            port_thread&.kill
            Main.unregister_live_app_ws(client_id)
        end

        ws.rack_response
    end
end
