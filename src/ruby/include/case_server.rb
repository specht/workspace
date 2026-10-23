require 'cgi'
require 'openssl'
require 'open3'
require 'tmpdir'
require 'fileutils'

# The case server of »Ein Krimi im Terminal« (docs/entwicklung.md, »Der
# Fallserver«): two small SSH servers, museum.bitstein and
# alarmanlage.bitstein, the second reachable only through the first. It is
# off unless CASE_SERVER = true in credentials.rb.
#
# Every person gets an account of their own, so one cannot end another's
# session, and the per-account limits on the servers hold for one person.
# The servers know no addresses: they have ACCOUNTS accounts e000 … e999,
# and the password of each comes from a secret both sides know. The website
# picks a person's account from their address; two people may share one,
# which does no harm on servers where nobody can write anything.
#
# The container computes the same password in its entrypoint with
#   printf '%s' "password:e042" | openssl dgst -sha256 -hmac "$CASE_SERVER_SECRET"
# and takes the first PASSWORD_LENGTH hex digits, so both stay in step.
module CaseServer
    ACCOUNTS = 1000
    PASSWORD_LENGTH = 12
    MUSEUM = 'museum.bitstein'
    ALARM = 'alarmanlage.bitstein'

    def self.enabled?
        defined?(CASE_SERVER) && CASE_SERVER == true
    end

    # The secret of the accounts, from a salt the installation already has.
    def self.secret(salt)
        OpenSSL::HMAC.hexdigest('SHA256', salt.to_s, 'case-server')
    end

    def self.login(secret, email)
        number = OpenSSL::HMAC.hexdigest('SHA256', secret, "login:#{email.to_s.downcase}").to_i(16) % ACCOUNTS
        format('e%03d', number)
    end

    def self.password(secret, login)
        OpenSSL::HMAC.hexdigest('SHA256', secret, "password:#{login}")[0, PASSWORD_LENGTH]
    end

    # The case files' address, and a person's own one: the page gives the
    # logged-in person a key in the path, so the website knows whose note to
    # add even when the Workspace fetches through the public address (and
    # the request arrives from the server's own address, not the
    # Workspace's). In the path, not after ?, so wget still saves
    # bitstein.tar.gz.
    ARCHIVE_PATH = '/terminal-mystery/bitstein.tar.gz'
    KEY_LENGTH = 16

    def self.archive_key(secret, email)
        OpenSSL::HMAC.hexdigest('SHA256', secret, "archive:#{email.to_s.downcase}")[0, KEY_LENGTH]
    end

    def self.personal_archive_path(secret, email)
        "/terminal-mystery/akten/#{archive_key(secret, email)}/bitstein.tar.gz"
    end

    # Whose key this is, among emails; nil if nobody's.
    def self.email_for_key(secret, key, emails)
        return nil unless key.to_s.match?(/\A[0-9a-f]{#{KEY_LENGTH}}\z/)

        emails.find { |email| archive_key(secret, email) == key }
    end

    # The variables a person's Workspace gets, for the screenshot recipes;
    # the page itself points to the note in the case files.
    def self.workspace_environment(secret, email)
        login = login(secret, email)
        { 'CASE_USER' => login, 'CASE_PASSWORD' => password(secret, login) }
    end

    NOTE_PATH = 'bitstein/zugang-museum.txt'

    # The note on the page (terminal-mystery.md, »Beweise vom Server«): one
    # person's user and password in place of CASE_NOTE_USER and
    # CASE_NOTE_PASSWORD. The page holds it, not only the case files: those
    # carry it only when they were fetched with the person's own address
    # while the case server was on.
    NOTE_USER = 'CASE_NOTE_USER'
    NOTE_PASSWORD = 'CASE_NOTE_PASSWORD'
    EXAMPLE_USER = 'e123'
    NOT_LOGGED_IN = '(nach dem Anmelden steht hier deins)'

    def self.note_values(secret, email)
        login = login(secret, email)
        { :user => login, :password => password(secret, login) }
    end

    # html with values (from note_values) in the note, or for nil the example
    # user and where the password will be.
    def self.fill_note(html, values)
        user = values ? values[:user] : EXAMPLE_USER
        password = values ? values[:password] : NOT_LOGGED_IN
        html.gsub(NOTE_USER, CGI.escapeHTML(user)).gsub(NOTE_PASSWORD, CGI.escapeHTML(password))
    end

    # The note in the case files, from the inspector: one person's access
    # to museum.bitstein.
    def self.note(secret, email)
        login = login(secret, email)
        <<~TEXT
            Kriminalpolizei Bitstein, Kommissarin Weber

            Das Stadtmuseum hat uns einen Zugang zu seinem Server gegeben,
            museum.bitstein. Dieser ist für dich:

                Benutzer   #{login}
                Passwort   #{password(secret, login)}

                ssh #{login}@museum.bitstein

            Gib ihn nicht weiter.
        TEXT
    end

    # The case archive (gzip) with the note appended to the tar inside; the
    # entries already in it stay byte for byte as they are.
    def self.archive_with_note(gzip, text, path: NOTE_PATH)
        Dir.mktmpdir('case-note') do |dir|
            tar, status = Open3.capture2('gzip', '-dc', :stdin_data => gzip, :binmode => true)
            raise 'gzip -d failed' unless status.success?

            File.binwrite(File.join(dir, 'case.tar'), tar)
            FileUtils.mkdir_p(File.join(dir, 'note', File.dirname(path)))
            File.write(File.join(dir, 'note', path), text)
            ok = system('tar', '--owner=0', '--group=0', '--numeric-owner', '--mode=u+rw,go+r,go-w',
                '-rf', File.join(dir, 'case.tar'), '-C', File.join(dir, 'note'), path)
            raise 'tar -r failed' unless ok

            out, status = Open3.capture2('gzip', '-n', '-c', File.join(dir, 'case.tar'), :binmode => true)
            raise 'gzip failed' unless status.success?

            out
        end
    end
end
