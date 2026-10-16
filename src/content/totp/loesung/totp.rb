# Einmalcodes wie in einer Authenticator-App: Musterlösung zu allen Abschnitten.
require 'openssl'
require 'securerandom'

# --- Base32 ------------------------------------------------------------------

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'

# Base32 in Bytes: jedes Zeichen sind 5 Bits, je 8 Bits ein Byte.
# Leerzeichen, = am Ende und Kleinbuchstaben sind erlaubt.
def base32_decode(text)
    bits = text.upcase.delete(' =').each_char.map do |c|
        value = ALPHABET.index(c)
        raise ArgumentError, "Kein Base32-Zeichen: #{c}" if value.nil?

        value.to_s(2).rjust(5, '0')
    end.join
    bits.scan(/.{8}/).map { |byte| byte.to_i(2) }.pack('C*')
end

# Bytes in Base32, ohne = am Ende.
def base32_encode(data)
    bits = data.bytes.map { |b| b.to_s(2).rjust(8, '0') }.join
    bits += '0' * (-bits.length % 5)
    bits.scan(/.{5}/).map { |group| ALPHABET[group.to_i(2)] }.join
end

# --- HOTP: ein Code für einen Zähler ------------------------------------------

def hotp(key, counter, digits = 6)
    hash = OpenSSL::HMAC.digest('SHA1', key, [counter].pack('Q>'))
    offset = hash.bytes.last & 15
    number = hash[offset, 4].unpack1('N') & 0x7fffffff
    (number % 10**digits).to_s.rjust(digits, '0')
end

# --- TOTP: der Zähler ist die Zeit ---------------------------------------------

def totp(key, time = Time.now.to_i, digits = 6, step = 30)
    hotp(key, time / step, digits)
end

# Stimmt der Code? Auch der aus dem Zeitfenster davor und danach zählt, weil
# Uhren nie genau gleich gehen.
def verify(key, code, time = Time.now.to_i, window = 1)
    (-window..window).any? { |d| totp(key, time + d * 30) == code.to_s.delete(' ') }
end

require 'rqrcode'
path = File.join(__dir__, 'secret.txt')
File.write(path, base32_encode(SecureRandom.random_bytes(20)) + "\n") unless File.exist?(path)
secret = File.read(path).strip
uri = "otpauth://totp/Hackschule:Workspace?secret=#{secret}&issuer=Hackschule"
RQRCode::QRCode.new(uri).as_png(size: 300).save(File.join(__dir__, 'qr.png'))
puts "Geheimnis: #{secret}"
puts 'Scanne qr.png mit deiner Authenticator-App. Beenden mit Strg+C.'
begin
    loop do
        now = Time.now.to_i
        print "\rCode: #{totp(base32_decode(secret), now)}, noch #{30 - now % 30} Sekunden "
        sleep 1
    end
rescue Interrupt
    puts
end
