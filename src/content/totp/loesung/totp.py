# Einmalcodes wie in einer Authenticator-App: Musterlösung zu allen Abschnitten.
import hashlib
import hmac
import os
import secrets
import time

# --- Base32 ------------------------------------------------------------------

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'


def base32_decode(text):
    """Base32 in Bytes: jedes Zeichen sind 5 Bits, je 8 Bits ein Byte.
    Leerzeichen, = am Ende und Kleinbuchstaben sind erlaubt."""
    bits = ''
    for c in text.upper().replace(' ', '').replace('=', ''):
        if c not in ALPHABET:
            raise ValueError(f'Kein Base32-Zeichen: {c}')
        bits += format(ALPHABET.index(c), '05b')
    return bytes(int(bits[i:i + 8], 2) for i in range(0, len(bits) - 7, 8))


def base32_encode(data):
    """Bytes in Base32, ohne = am Ende."""
    bits = ''.join(format(b, '08b') for b in data)
    bits += '0' * (-len(bits) % 5)
    return ''.join(ALPHABET[int(bits[i:i + 5], 2)] for i in range(0, len(bits), 5))


# --- HOTP: ein Code für einen Zähler ------------------------------------------

def hotp(key, counter, digits=6):
    hash = hmac.new(key, counter.to_bytes(8, 'big'), hashlib.sha1).digest()
    offset = hash[-1] & 15
    number = int.from_bytes(hash[offset:offset + 4], 'big') & 0x7fffffff
    return str(number % 10 ** digits).zfill(digits)


# --- TOTP: der Zähler ist die Zeit ---------------------------------------------

def totp(key, now=None, digits=6, step=30):
    now = int(time.time()) if now is None else now
    return hotp(key, now // step, digits)


def verify(key, code, now=None, window=1):
    """Stimmt der Code? Auch der aus dem Zeitfenster davor und danach zählt,
    weil Uhren nie genau gleich gehen."""
    now = int(time.time()) if now is None else now
    return any(totp(key, now + d * 30) == str(code).replace(' ', '') for d in range(-window, window + 1))


if __name__ == '__main__':
    import qrcode
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'secret.txt')
    if not os.path.exists(path):
        with open(path, 'w') as f:
            f.write(base32_encode(secrets.token_bytes(20)) + '\n')
    with open(path) as f:
        secret = f.read().strip()
    uri = f'otpauth://totp/Hackschule:Workspace?secret={secret}&issuer=Hackschule'
    qrcode.make(uri).save(os.path.join(os.path.dirname(path), 'qr.png'))
    print(f'Geheimnis: {secret}')
    print('Scanne qr.png mit deiner Authenticator-App. Beenden mit Strg+C.')
    try:
        while True:
            now = int(time.time())
            print(f'\rCode: {totp(base32_decode(secret), now)}, noch {30 - now % 30} Sekunden ', end='', flush=True)
            time.sleep(1)
    except KeyboardInterrupt:
        print()
