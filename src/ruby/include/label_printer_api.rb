require 'open3'
require_relative 'label_printer'

# The server's part of the labels (LabelPrinter, docs/etiketten.md):
# the switch and the queue for the administration (/etiketten-station),
# printing for visitors (/etiketten, with the key of the QR code, no
# account), and the two calls of the station on the laptop (with its
# token). Who drew a label is not stored; a label goes after two days, one
# with a photo as soon as it is printed (or after two hours unprinted).
class Main < Sinatra::Base
    LABEL_SETTING = 'labels'

    # the last label of each browser (a random id the page keeps), in memory
    @@label_browsers = {}
    @@label_station_seen = nil
    @@label_mutex = Mutex.new

    def label_setting
        row = neo4j_query('MATCH (s:Setting {key: $key}) RETURN s.until AS until, s.visitor_key AS visitor_key, s.station_token AS station_token;',
                          :key => LABEL_SETTING).first
        row && { :until => row['until'].to_i, :visitor_key => row['visitor_key'], :station_token => row['station_token'] }
    end

    def label_url(setting) = "#{WEB_ROOT}/etiketten?k=#{setting[:visitor_key]}"

    def label_qr_svg(url)
        svg, _error, status = Open3.capture3('qrencode', '-t', 'SVG', '-m', '2', '-o', '-', :stdin_data => url)
        status.success? ? svg.sub(/\A<\?xml[^>]*>\s*/, '') : nil
    rescue Errno::ENOENT
        nil
    end

    def label_jobs_cleanup
        now = Time.now.to_i
        neo4j_query('MATCH (j:LabelJob) WHERE j.ts < $before OR (j.photo AND j.ts < $photos) DELETE j;',
                    :before => now - LabelPrinter::KEEP_SECONDS, :photos => now - LabelPrinter::PHOTO_SECONDS)
    end

    def label_visitor!(key)
        setting = label_setting
        halt 403, { :error => 'labels_off' }.to_json unless LabelPrinter.on?(setting)
        halt 403, { :error => 'wrong_key' }.to_json unless key.to_s.size >= 16 && Rack::Utils.secure_compare(key.to_s, setting[:visitor_key].to_s)
        setting
    end

    def label_station!(token)
        setting = label_setting
        ok = setting && token.to_s.size >= 16 && Rack::Utils.secure_compare(token.to_s, setting[:station_token].to_s)
        halt 403, { :error => 'wrong_token' }.to_json unless ok
        @@label_station_seen = Time.now.to_i
        setting
    end

    # The switch: on for some hours (new keys each time, so that last
    # time's QR code no longer prints) or off
    post '/api/labels/switch' do
        admins_only!
        data = parse_request_data(:required_keys => [:hours], :types => { :hours => Integer })
        hours = data[:hours]
        halt 400 unless hours.zero? || LabelPrinter::HOURS.include?(hours)
        if hours.zero?
            neo4j_query('MERGE (s:Setting {key: $key}) SET s.until = 0;', :key => LABEL_SETTING)
        else
            neo4j_query('MERGE (s:Setting {key: $key}) SET s.until = $until, s.visitor_key = $visitor, s.station_token = $station;',
                        :key => LABEL_SETTING, :until => Time.now.to_i + hours * 3600,
                        :visitor => RandomTag.generate(24), :station => RandomTag.generate(32))
        end
        respond(:ok => true)
    end

    # What the station page shows: the switch, the QR code, the station,
    # the labels of the last two days with their pictures
    post '/api/labels/state' do
        admins_only!
        label_jobs_cleanup
        setting = label_setting
        on = LabelPrinter.on?(setting)
        jobs = neo4j_query('MATCH (j:LabelJob) RETURN j.id AS id, j.ts AS ts, j.state AS state, j.bits AS bits, j.photo AS photo ORDER BY j.ts DESC LIMIT 60;').map do |r|
            { :id => r['id'], :ts => r['ts'].to_i, :state => r['state'], :bits => r['bits'], :photo => r['photo'] == true }
        end
        url = on ? label_url(setting) : nil
        respond(:on => on, :until => on ? setting[:until] : nil, :url => url, :qr => url && label_qr_svg(url),
                :token => on ? setting[:station_token] : nil, :server => WEB_ROOT,
                :station_seen => @@label_station_seen, :now => Time.now.to_i, :jobs => jobs,
                :hours => LabelPrinter::HOURS)
    end

    post '/api/labels/delete' do
        admins_only!
        data = parse_request_data(:required_keys => [:id])
        neo4j_query('MATCH (j:LabelJob {id: $id}) DELETE j;', :id => data[:id])
        respond(:ok => true)
    end

    # once more: back into the queue
    post '/api/labels/again' do
        admins_only!
        data = parse_request_data(:required_keys => [:id])
        neo4j_query("MATCH (j:LabelJob {id: $id}) SET j.state = 'queued', j.ts = $ts;", :id => data[:id], :ts => Time.now.to_i)
        respond(:ok => true)
    end

    # Whether the page may print: on, and the key of the QR code
    post '/api/labels/open' do
        data = parse_request_data(:optional_keys => [:key])
        setting = label_setting
        respond(:on => LabelPrinter.on?(setting),
                :key_ok => LabelPrinter.on?(setting) && Rack::Utils.secure_compare(data[:key].to_s, setting[:visitor_key].to_s))
    end

    post '/api/labels/print' do
        data = parse_request_data(:required_keys => [:key, :browser, :bits], :optional_keys => [:photo],
                                  :types => { :photo => TrueClass },
                                  :max_body_length => LabelPrinter::BYTES * 2 + 1024,
                                  :max_value_lengths => { :bits => LabelPrinter::BYTES * 2 })
        label_visitor!(data[:key])
        bits = data[:bits]
        halt 400, { :error => 'bad_picture' }.to_json unless LabelPrinter.valid_bits?(bits)
        halt 400, { :error => 'blank' }.to_json if LabelPrinter.blank?(bits)
        browser = data[:browser].to_s[0, 64]
        now = Time.now.to_i
        wait = @@label_mutex.synchronize do
            @@label_browsers.delete_if { |_, t| t < now - 3600 }
            w = LabelPrinter.browser_wait(@@label_browsers, browser, now)
            @@label_browsers[browser] = now if w.zero?
            w
        end
        halt 429, { :error => 'wait', :seconds => wait }.to_json if wait.positive?
        queued = neo4j_query("MATCH (j:LabelJob {state: 'queued'}) RETURN count(j) AS n;").first['n'].to_i
        halt 429, { :error => 'busy' }.to_json if queued >= LabelPrinter::MAX_QUEUED
        id = RandomTag.generate(16)
        label_jobs_cleanup
        neo4j_query("CREATE (:LabelJob {id: $id, ts: $ts, state: 'queued', bits: $bits, photo: $photo});",
                    :id => id, :ts => now, :bits => bits, :photo => data[:photo] == true)
        respond(:id => id, :ahead => queued)
    end

    # How far a label is: queued (with how many before it), printing,
    # printed, or gone
    post '/api/labels/status' do
        data = parse_request_data(:required_keys => [:key, :id])
        label_visitor!(data[:key])
        row = neo4j_query('MATCH (j:LabelJob {id: $id}) RETURN j.state AS state, j.ts AS ts;', :id => data[:id]).first
        halt 404, { :error => 'gone' }.to_json unless row
        ahead = neo4j_query("MATCH (j:LabelJob {state: 'queued'}) WHERE j.ts < $ts RETURN count(j) AS n;", :ts => row['ts']).first['n'].to_i
        station = !@@label_station_seen.nil? && @@label_station_seen > Time.now.to_i - LabelPrinter::STATION_SECONDS
        respond(:state => row['state'], :ahead => ahead, :station => station)
    end

    # The station: the oldest waiting label as ZPL, then »done«
    post '/api/labels/next' do
        data = parse_request_data(:required_keys => [:token])
        label_station!(data[:token])
        row = neo4j_query(<<~END_OF_QUERY).first
            MATCH (j:LabelJob {state: 'queued'})
            WITH j ORDER BY j.ts LIMIT 1
            SET j.state = 'printing'
            RETURN j.id AS id, j.bits AS bits;
        END_OF_QUERY
        respond(row ? { :id => row['id'], :zpl => LabelPrinter.zpl(row['bits']) } : { :id => nil })
    end

    post '/api/labels/done' do
        data = parse_request_data(:required_keys => [:token, :id])
        label_station!(data[:token])
        # with a photo it goes at once; the page then hears »gone«
        neo4j_query("MATCH (j:LabelJob {id: $id}) WHERE j.photo DELETE j;", :id => data[:id])
        neo4j_query("MATCH (j:LabelJob {id: $id}) SET j.state = 'printed';", :id => data[:id])
        respond(:ok => true)
    end

    # The station's script, for the laptop
    get '/api/labels/label-station.rb' do
        admins_only!
        respond_raw_with_mimetype_and_filename(File.read(File.join(File.dirname(__FILE__), '..', '..', 'scripts', 'label-station.rb')),
                                               'text/x-ruby', 'label-station.rb')
    end
end
