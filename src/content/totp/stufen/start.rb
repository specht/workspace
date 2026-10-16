# Einmalcodes wie in einer Authenticator-App
require 'openssl'
require 'securerandom'

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'

# Base32 in Bytes: jedes Zeichen sind 5 Bits, je 8 Bits ein Byte.
def base32_decode(text)
    ''.b
end

# Bytes in Base32, ohne = am Ende.
def base32_encode(data)
    ''
end

# Der Code für einen Zähler.
def hotp(key, counter, digits = 6)
    '000000'
end

if __FILE__ == $0
    p base32_decode('JBSWY3DPEHPK3PXP')
end
