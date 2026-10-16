# Einmalcodes wie in einer Authenticator-App
import hashlib
import hmac
import os
import secrets
import time

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'


def base32_decode(text):
    """Base32 in Bytes: jedes Zeichen sind 5 Bits, je 8 Bits ein Byte."""
    return b''


def base32_encode(data):
    """Bytes in Base32, ohne = am Ende."""
    return ''


def hotp(key, counter, digits=6):
    """Der Code für einen Zähler."""
    return '000000'


if __name__ == '__main__':
    print(base32_decode('JBSWY3DPEHPK3PXP'))
