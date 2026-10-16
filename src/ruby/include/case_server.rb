require 'openssl'

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

    # The variables a person's Workspace gets: ssh $CASE_USER@museum.bitstein
    def self.workspace_environment(secret, email)
        login = login(secret, email)
        { 'CASE_USER' => login, 'CASE_PASSWORD' => password(secret, login) }
    end
end
