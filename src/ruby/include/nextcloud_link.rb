require 'ipaddr'
require 'json'
require 'net/http'
require 'socket'
require 'uri'

# Documents from Nextcloud Office through a share link
# (docs/plan-textverarbeitung.md, »Nextcloud Office through a share link«).
#
# The teacher of a group names the school's Nextcloud on the class page
# (GroupSetting.nextcloud); the Workspace asks it for status.php before it
# keeps the address. A student of the group pastes a public link to a
# document (https://cloud.example.org/s/<token>) into a document card, and
# each check fetches the current file (…/s/<token>/download) instead of an
# upload.
#
# The server only ever fetches from the address the teacher gave: https on
# port 443, every address the name resolves to public (never the Workspace's
# own network, a school's LAN or loopback), the connection made to the
# address that was checked, redirects only within the same Nextcloud, a size
# limit and short timeouts. Nothing is sent but the token.
module NextcloudLink
    # Something the person can fix: the message says what.
    class Refused < StandardError; end

    OPEN_TIMEOUT = 5
    READ_TIMEOUT = 20
    # the whole fetch, however slowly the bytes come
    DEADLINE = 60
    MAX_REDIRECTS = 3
    TOKEN = /\A[A-Za-z0-9]{8,64}\z/
    LINK_FILE = '.nextcloud-link'
    # never fetched from: besides private, loopback and link-local
    BLOCKED = %w[0.0.0.0/8 100.64.0.0/10 192.0.0.0/24 192.0.2.0/24 198.18.0.0/15 198.51.100.0/24
                 203.0.113.0/24 224.0.0.0/4 240.0.0.0/4 ::/128 ::1/128 64:ff9b::/96 100::/64 2001:db8::/32
                 ff00::/8].map { |net| IPAddr.new(net) }.freeze

    # The address of a Nextcloud as typed by a teacher, as
    # https://host[/path] without a slash at the end; raises Refused.
    # Takes the address of any page of it (…/apps/files/…, …/login).
    def self.server(text)
        text = text.to_s.strip
        text = "https://#{text}" unless text =~ %r{\A[a-z]+://}i
        uri = URI.parse(text)
        raise Refused, 'Die Adresse muss mit https:// beginnen.' unless uri.scheme&.downcase == 'https'
        raise Refused, 'In der Adresse fehlt der Name des Servers.' if uri.host.to_s.empty?
        raise Refused, 'Die Adresse darf keinen Namen und kein Passwort enthalten.' if uri.userinfo
        raise Refused, 'Der Workspace holt Dokumente nur über den üblichen Port 443.' unless uri.port == 443
        host = uri.host.downcase
        raise Refused, 'Gib den Namen des Servers an, keine IP-Adresse.' if ip?(host)
        raise Refused, 'Das ist kein vollständiger Name eines Servers.' unless host =~ /\A[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+\z/
        # the part before Nextcloud's own paths: a Nextcloud under /cloud
        path = uri.path.to_s.sub(%r{/(index\.php|apps|login|s|f|settings|status\.php)(/.*)?\z}, '').sub(%r{/+\z}, '')
        raise Refused, 'Die Adresse enthält Zeichen, die in einem Pfad nicht vorkommen.' unless path =~ %r{\A(/[A-Za-z0-9._~-]+)*\z}
        "https://#{host}#{path}"
    rescue URI::Error
        raise Refused, 'Das ist keine Adresse, die ich lesen kann.'
    end

    # The token of a share link of that Nextcloud; raises Refused.
    def self.token(server, link)
        link = link.to_s.strip
        raise Refused, "Füge den Link aus eurer Nextcloud ein: #{server}/s/…" if link.empty?
        link = "https://#{link}" unless link =~ %r{\A[a-z]+://}i
        uri = URI.parse(link)
        base = URI.parse(server)
        unless uri.scheme&.downcase == 'https' && uri.host&.downcase == base.host && uri.port == 443
            raise Refused, "Das ist kein Link aus eurer Nextcloud. Ein Link zum Teilen beginnt mit #{server}/s/"
        end
        path = uri.path.to_s
        rest = base.path.empty? || path.start_with?("#{base.path}/") ? path.delete_prefix(base.path) : ''
        m = rest.match(%r{\A(?:/index\.php)?/s/([^/]+)(?:/download)?/?\z})
        raise Refused, "Das ist kein Link zum Teilen. Er beginnt mit #{server}/s/ (in Nextcloud: Teilen, »Öffentlicher Link«)." unless m && m[1] =~ TOKEN
        m[1]
    rescue URI::Error
        raise Refused, 'Das ist kein Link, den ich lesen kann.'
    end

    # What the Nextcloud says about itself (status.php): { product:,
    # version: }; raises Refused with what is wrong.
    def self.verify(server)
        body = nil
        get("#{server}/status.php", :max => 64 * 1024) do |response, text|
            raise Refused, "Unter #{server} antwortet keine Nextcloud (#{response.code}). Stimmt die Adresse? Sie ist dieselbe wie im Browser vor /apps/…" unless response.code == '200'
            body = text
        end
        info = JSON.parse(body.to_s) rescue nil
        unless info.is_a?(Hash) && info.key?('installed') && info['version']
            raise Refused, "Unter #{server} antwortet keine Nextcloud. Stimmt die Adresse? Sie ist dieselbe wie im Browser vor /apps/…"
        end
        raise Refused, "Die Nextcloud unter #{server} ist nicht fertig eingerichtet." unless info['installed']
        { :product => (info['productname'] || 'Nextcloud').to_s[0, 60], :version => (info['versionstring'] || info['version']).to_s[0, 30],
          :maintenance => !!info['maintenance'] }
    end

    # The shared file: [name, bytes]; raises Refused with what went wrong.
    def self.fetch(server, token, max_bytes:)
        raise Refused, 'Das ist kein Link zum Teilen.' unless token =~ TOKEN

        result = nil
        get("#{server}/s/#{token}/download", :max => max_bytes, :same => server) do |response, body|
            case response.code
            when '200'
                type = response['content-type'].to_s
                name = filename(response['content-disposition'])
                if type.start_with?('text/html') || name.nil?
                    raise Refused, 'Hinter dem Link steht keine Datei, die ich laden kann. Ist er mit einem Passwort geschützt oder das Herunterladen ausgeschaltet? ' \
                        'Teile das Dokument ohne Passwort und lass »Herunterladen verbergen« aus.'
                end
                raise Refused, 'Der Link zeigt auf einen Ordner. Teile nur das Dokument selbst.' if name =~ /\.zip\z/i && type =~ /zip/
                result = [name, body.b]
            when '401', '403'
                raise Refused, 'Die Nextcloud lässt mich das Dokument nicht laden. Ist der Link mit einem Passwort geschützt? Teile es ohne Passwort.'
            when '404', '410'
                raise Refused, 'Diesen Link gibt es in eurer Nextcloud nicht (mehr). Ist er abgelaufen oder gelöscht? Teile das Dokument noch einmal und füge den neuen Link ein.'
            when '503'
                raise Refused, 'Eure Nextcloud wird gerade gewartet. Versuch es gleich noch einmal.'
            else
                raise Refused, "Eure Nextcloud antwortet mit einem Fehler (#{response.code}). Versuch es gleich noch einmal."
            end
        end
        result
    end

    # The file's name from Content-Disposition (filename* before filename),
    # without folders; nil without one.
    def self.filename(header)
        header = header.to_s
        name = if (m = header.match(/filename\*\s*=\s*(?:UTF-8|utf-8)''([^;]+)/))
            URI.decode_www_form_component(m[1].strip.delete('"'))
        elsif (m = header.match(/filename\s*=\s*"([^"]*)"/) || header.match(/filename\s*=\s*([^;]+)/))
            m[1].strip
        end
        return nil if name.nil? || name.empty?

        File.basename(name.force_encoding('UTF-8').scrub.tr('\\', '/'))[0, 200]
    end

    def self.ip?(host)
        IPAddr.new(host.delete('[]'))
        true
    rescue IPAddr::Error
        false
    end

    def self.public_ip?(ip)
        ip = IPAddr.new(ip) unless ip.is_a?(IPAddr)
        ip = ip.native if ip.ipv4_mapped? || ip.ipv4_compat?
        return false if ip.private? || ip.loopback? || ip.link_local?
        return false if ip.ipv6? && IPAddr.new('fc00::/7').include?(ip)

        BLOCKED.none? { |net| net.family == ip.family && net.include?(ip) }
    end

    # The addresses of host; raises Refused if any of them is not public
    # (a name that also points into a private network is never used).
    def self.addresses(host)
        # IPv4 first: a container may have no IPv6 route
        found = Addrinfo.getaddrinfo(host, 443, nil, :STREAM).sort_by { |a| a.ipv4? ? 0 : 1 }.map(&:ip_address).uniq
        raise Refused, "Den Server #{host} finde ich nicht. Stimmt der Name?" if found.empty?
        raise Refused, "#{host} zeigt in ein privates Netz. Der Workspace holt Dokumente nur aus dem Internet." unless found.all? { |ip| public_ip?(ip) }

        found
    rescue SocketError
        raise Refused, "Den Server #{host} finde ich nicht. Stimmt der Name?"
    end

    # GETs url (and redirects within same, a server address), yielding the
    # last response and its body, read up to max bytes.
    def self.get(url, max:, same: nil)
        hops = 0
        until_time = Process.clock_gettime(Process::CLOCK_MONOTONIC) + DEADLINE
        loop do
            uri = URI.parse(url)
            ip = addresses(uri.host).first
            http = Net::HTTP.new(uri.host, 443)
            http.ipaddr = ip
            http.use_ssl = true
            http.verify_mode = OpenSSL::SSL::VERIFY_PEER
            http.open_timeout = OPEN_TIMEOUT
            http.read_timeout = READ_TIMEOUT
            http.ssl_timeout = OPEN_TIMEOUT
            redirect = nil
            http.start do
                request = Net::HTTP::Get.new(uri.request_uri, 'User-Agent' => 'Workspace (Office-Prüfung)', 'Accept-Encoding' => 'identity')
                http.request(request) do |response|
                    if response.is_a?(Net::HTTPRedirection)
                        redirect = URI.join(url, response['location'].to_s).to_s
                        next
                    end
                    if response['content-length'] && response['content-length'].to_i > max
                        raise Refused, "Die Datei ist größer als #{max / 1024 / 1024} MB. Sind große Bilder darin?"
                    end
                    body = +''
                    response.read_body do |chunk|
                        body << chunk
                        raise Refused, 'Die Nextcloud schickt das Dokument zu langsam. Versuch es gleich noch einmal.' if Process.clock_gettime(Process::CLOCK_MONOTONIC) > until_time
                        raise Refused, "Die Datei ist größer als #{max / 1024 / 1024} MB. Sind große Bilder darin?" if body.bytesize > max
                    end
                    yield response, body
                end
            end
            return if redirect.nil?

            hops += 1
            target = URI.parse(redirect)
            base = URI.parse(same || url)
            inside = target.scheme == 'https' && target.host&.downcase == base.host && target.port == 443 &&
                target.path.to_s.start_with?(same ? "#{base.path}/" : '/')
            # a share behind a password sends to its page: no file there
            raise Refused, 'Hinter dem Link steht keine Datei, die ich laden kann. Ist er mit einem Passwort geschützt? Teile das Dokument ohne Passwort.' if same && inside && target.path =~ %r{/s/[^/]+/?(authenticate.*)?\z}
            raise Refused, 'Die Nextcloud leitet auf einen anderen Server weiter. Stimmt die Adresse?' unless inside && hops <= MAX_REDIRECTS

            url = redirect
        end
    rescue Net::OpenTimeout, Net::ReadTimeout
        raise Refused, 'Die Nextcloud antwortet nicht rechtzeitig. Versuch es gleich noch einmal.'
    rescue OpenSSL::SSL::SSLError
        raise Refused, 'Die Verbindung zur Nextcloud ist nicht sicher (Zertifikat). Stimmt die Adresse?'
    rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH, Errno::ENETUNREACH, Errno::ECONNRESET, EOFError, IOError
        raise Refused, 'Die Nextcloud ist gerade nicht erreichbar. Versuch es gleich noch einmal.'
    end
end
