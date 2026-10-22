require_relative 'cable_ether'

# »Datenübertragung per Kabel«: the lamps of the activity /cable and of the
# short address /ether (static/ether.html) talk to one CableEther over
# /ws_cable. No login: the second room may be a phone without an account.
# Instead: only pages of this site connect (Origin), at most
# CableEther::MAX_CLIENTS connections and MAX_PER_ADDRESS per address, and
# the ether limits what one connection may send. Teachers and administrators
# (from the session, here) may change the number of lamps for everyone.
# Nothing is stored; the per-address count lives in memory only.
class Main < Sinatra::Base
    CABLE_ETHER = CableEther.new
    CABLE_ADDRESS_PATH = '/ether'

    get '/ws_cable' do
        halt 400 unless Faye::WebSocket.websocket?(request.env)
        origin = request.env['HTTP_ORIGIN'].to_s
        halt 403 if origin.empty? || !Authentication.same_origin?(:origin => origin, :referer => nil, :expected_origin => WEB_ROOT)

        Main.load_invitations()
        teacher = teacher_logged_in? || teacher_account_logged_in?
        address = client_ip
        # Faye pings every 25 s: nginx ends a websocket that stays silent for a minute.
        ws = Faye::WebSocket.new(request.env, nil, :ping => 25)
        id = nil
        ws.on(:open) do |_|
            id = CABLE_ETHER.join(->(message) { ws.send(message.to_json) }, :teacher => teacher, :address => address)
            ws.close(4001, 'full') if id.nil?
        end
        ws.on(:message) { |message| CABLE_ETHER.receive(id, message.data) if id }
        ws.on(:close) { |_| CABLE_ETHER.leave(id) if id }
        ws.rack_response
    end

    # The QR code of the short address, for the projector (»Für den Beamer«).
    @@cable_qr_svg = nil

    get '/api/cable/qr.svg' do
        @@cable_qr_svg ||= begin
            svg, error, status = Open3.capture3('qrencode', '-t', 'SVG', '-m', '2', '-o', '-', :stdin_data => "#{WEB_ROOT}#{CABLE_ADDRESS_PATH}")
            raise "qrencode failed: #{error}" unless status.success?

            svg
        end
        headers 'Cache-Control' => 'public, max-age=3600'
        respond_raw_with_mimetype(@@cable_qr_svg, 'image/svg+xml')
    end
end
