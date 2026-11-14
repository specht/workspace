require 'base64'
require 'openssl'
require 'securerandom'
require 'uri'

# Signing in with an Authenticator app (RFC 6238, TOTP), as a second way
# beside the code by e-mail: the person sets it up in the profile with a QR
# code, and the login page then takes the six digits the app shows instead
# of waiting for an e-mail. The secret is stored encrypted (AES-256-GCM, its
# key from LOGIN_CODE_SALT in credentials.rb, outside the database), so a
# dump of the database alone signs nobody in. Each code counts once
# (the step of the last one used is kept), a step before or after the
# present one is taken too (clocks off by up to 30 seconds).
# Plain Ruby, tested on its own (totp_test.rb); authenticator_page.rb wires
# it into Main.
module Totp
    STEP = 30
    DIGITS = 6
    BASE32 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'.freeze

    # A new secret: 20 random bytes (160 bits, RFC 4226) in Base32
    def self.new_secret
        base32(SecureRandom.random_bytes(20))
    end

    def self.base32(bytes)
        bits = bytes.unpack1('B*')
        bits += '0' * ((5 - bits.size % 5) % 5)
        bits.scan(/.{5}/).map { |b| BASE32[b.to_i(2)] }.join
    end

    def self.unbase32(text)
        bits = text.to_s.upcase.delete('= ').chars.map do |c|
            i = BASE32.index(c)
            raise ArgumentError, 'not Base32' unless i

            i.to_s(2).rjust(5, '0')
        end.join
        [bits[0, bits.size - bits.size % 8]].pack('B*')
    end

    # The code of a step (RFC 4226 with the step as the counter)
    def self.code(secret, step, digits: DIGITS)
        mac = OpenSSL::HMAC.digest('SHA1', unbase32(secret), [step].pack('Q>'))
        offset = mac.bytes.last & 0x0f
        number = mac[offset, 4].unpack1('N') & 0x7fffffff
        (number % (10**digits)).to_s.rjust(digits, '0')
    end

    def self.step_at(time)
        time.to_i / STEP
    end

    # The step a code belongs to (the present one, one before, one after),
    # never one at or before the last step used; nil when it fits none
    def self.verify(secret, given, now: Time.now, last_step: nil)
        return nil unless given.is_a?(String) && given.match?(/\A\d{#{DIGITS}}\z/)

        present = step_at(now)
        [present, present - 1, present + 1].find do |step|
            next false if last_step && step <= last_step.to_i

            OpenSSL.fixed_length_secure_compare(code(secret, step), given)
        end
    end

    # What the app scans: otpauth://totp/…, the label the site and the address
    def self.uri(secret, email, issuer:)
        label = URI.encode_www_form_component("#{issuer}:#{email}").gsub('+', '%20')
        query = URI.encode_www_form(:secret => secret, :issuer => issuer, :algorithm => 'SHA1', :digits => DIGITS, :period => STEP)
        "otpauth://totp/#{label}?#{query}"
    end

    # The secret in groups of four, to type into an app that cannot scan
    def self.grouped(secret)
        secret.scan(/.{1,4}/).join(' ')
    end

    def self.key(salt)
        OpenSSL::Digest::SHA256.digest("totp-secret:#{salt}")
    end

    # Encrypted for the database: »v1:« and Base64 of nonce, tag and text
    def self.encrypt(secret, salt)
        cipher = OpenSSL::Cipher.new('aes-256-gcm').encrypt
        cipher.key = key(salt)
        nonce = cipher.random_iv
        text = cipher.update(secret) + cipher.final
        'v1:' + Base64.strict_encode64(nonce + cipher.auth_tag + text)
    end

    # The secret, or nil when it cannot be read (another salt, damaged):
    # then the app does not sign in, the e-mail still does
    def self.decrypt(stored, salt)
        return nil unless stored.is_a?(String) && stored.start_with?('v1:')

        raw = Base64.strict_decode64(stored[3..])
        cipher = OpenSSL::Cipher.new('aes-256-gcm').decrypt
        cipher.key = key(salt)
        cipher.iv = raw[0, 12]
        cipher.auth_tag = raw[12, 16]
        cipher.update(raw[28..]) + cipher.final
    rescue ArgumentError, OpenSSL::Cipher::CipherError
        nil
    end
end
