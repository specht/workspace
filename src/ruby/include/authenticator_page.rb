require 'open3'
require_relative 'totp'

# Signing in with an Authenticator app, in the profile (»Konto &
# Darstellung«, below »Wo du angemeldet bist«): setting it up with a QR
# code and one code from the app to confirm, and switching it off. The
# login itself is in login_page.rb (request_login says whether the account
# has an app and sends no e-mail then; complete_login takes the app's code
# too). The logic is in totp.rb. Never while someone else looks into the
# account (impersonation).
class Main < Sinatra::Base
    # The app's label: the site as its people know it
    def authenticator_issuer
        "Workspace #{WEBSITE_HOST.split(':').first}"
    end

    def authenticator_state
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.totp_secret AS secret, u.totp_since AS since;',
                          :email => @session_user[:email]).first || {}
        { :on => !row['secret'].to_s.empty?, :since => row['since'] }
    end

    def authenticator_html
        return '' unless user_logged_in? && @impersonated_by.nil?

        state = authenticator_state
        since = state[:since] ? Time.at(state[:since].to_i).strftime('%d.%m.%Y') : nil
        body = if state[:on]
            "<p class='authenticator-on'><i class='bi bi-shield-check text-success' aria-hidden='true'></i>" \
                "Eingerichtet#{since ? " am #{since}" : ''}. Beim Anmelden fragt der Workspace nach dem Code aus deiner App; " \
                'mit einem Code per E-Mail geht es weiterhin.</p>' \
                "<button type='button' id='bu_authenticator_off' class='btn btn-sm btn-outline-danger'><i class='bi bi-x-lg'></i>Ausschalten</button>"
        else
            "<p>Mit einer Authenticator-App auf deinem Handy (etwa Google Authenticator, Microsoft Authenticator oder 2FAS) " \
                'meldest du dich mit einem Code aus der App an, ohne auf eine E-Mail zu warten. Der Code per E-Mail geht weiterhin.</p>' \
                "<button type='button' id='bu_authenticator_start' class='btn btn-sm btn-outline-secondary'><i class='bi bi-phone'></i>Einrichten</button>" \
                "<div id='authenticator_setup' class='authenticator-setup' hidden>" \
                "<img id='authenticator_qr' alt='QR-Code für die Authenticator-App'>" \
                "<div class='authenticator-steps'>" \
                "<ol><li>Öffne deine Authenticator-App und scanne den QR-Code. Bist du auf dem Handy, tippe <a id='authenticator_link' href='#'>hier</a>.</li>" \
                "<li>Kann die App nicht scannen, gib diesen Schlüssel ein: <code id='authenticator_secret'></code></li>" \
                '<li>Gib den sechsstelligen Code ein, den die App jetzt zeigt.</li></ol>' \
                "<div class='input-group input-group-sm authenticator-confirm'>" \
                "<input type='text' id='ti_authenticator_code' class='form-control' placeholder='6-stelliger Code' aria-label='Code aus der App' inputmode='numeric' maxlength='6' autocomplete='one-time-code'>" \
                "<button type='button' id='bu_authenticator_confirm' class='btn btn-primary'><i class='bi bi-check-lg'></i>Bestätigen</button>" \
                "<button type='button' id='bu_authenticator_cancel' class='btn btn-outline-secondary'>Abbrechen</button></div>" \
                "<p id='authenticator_message' class='small text-danger mt-2 mb-0' role='status'></p></div></div>"
        end
        "<h3 class='mt-4' id='authenticator'>Anmelden mit einer Authenticator-App</h3>#{body}"
    end

    # A new secret, kept as pending until a code from the app confirms it;
    # the QR code (SVG) and the secret to type in
    post '/api/authenticator/start' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        secret = Totp.new_secret
        uri = Totp.uri(secret, @session_user[:email], :issuer => authenticator_issuer)
        svg, error, status = Open3.capture3('qrencode', '-t', 'SVG', '-m', '2', '-o', '-', :stdin_data => uri)
        unless status.success?
            STDERR.puts ">>> qrencode failed: #{error}"
            halt 503, { :error => 'qr_unavailable' }.to_json
        end
        neo4j_query('MATCH (u:User {email: $email}) SET u.totp_pending = $pending, u.totp_pending_at = $now;',
                    :email => @session_user[:email], :pending => Totp.encrypt(secret, LOGIN_CODE_SALT), :now => Time.now.to_i)
        respond(:qr => "data:image/svg+xml;base64,#{Base64.strict_encode64(svg)}", :secret => Totp.grouped(secret), :uri => uri)
    end

    # The first code from the app: the pending secret becomes the account's
    post '/api/authenticator/confirm' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        halt 429, { :error => 'rate_limited' }.to_json unless DEVELOPMENT || LOGIN_TRIES_PER_CLIENT.allow?(client_ip)
        data = parse_request_data(:required_keys => [:code], :max_value_lengths => { :code => 12 })
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.totp_pending AS pending, u.totp_pending_at AS at;',
                          :email => @session_user[:email]).first || {}
        secret = Totp.decrypt(row['pending'], LOGIN_CODE_SALT)
        # a setup left open for an hour starts again
        halt 409, { :error => 'no_setup' }.to_json if secret.nil? || Time.now.to_i - row['at'].to_i > 3600
        step = Totp.verify(secret, data[:code].to_s.delete(' '))
        halt 401, { :error => 'wrong_code' }.to_json unless step
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :secret => row['pending'], :now => Time.now.to_i, :step => step)
            MATCH (u:User {email: $email})
            SET u.totp_secret = $secret, u.totp_since = $now, u.totp_last_step = $step
            REMOVE u.totp_pending, u.totp_pending_at;
        END_OF_QUERY
        STDERR.puts "#{@session_user[:email]} set up an Authenticator app"
        respond(:ok => true)
    end

    post '/api/authenticator/off' do
        assert(user_logged_in?)
        halt 403, { :error => 'impersonating' }.to_json if @impersonated_by
        neo4j_query('MATCH (u:User {email: $email}) REMOVE u.totp_secret, u.totp_since, u.totp_last_step, u.totp_pending, u.totp_pending_at;',
                    :email => @session_user[:email])
        STDERR.puts "#{@session_user[:email]} switched off the Authenticator app"
        respond(:ok => true)
    end
end
