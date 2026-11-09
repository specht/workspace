"""A test call (runs in the phone-testclient image): baresip dials, presses
keys at given seconds, records what it hears into /out.
    python3 call.py EXTENSION SECONDS KEY@SECOND ..."""
import socket
import subprocess
import sys
import threading
import time

extension, seconds = sys.argv[1], float(sys.argv[2])
keys = [(float(at), key) for key, at in (k.split('@') for k in sys.argv[3:])]
phone = subprocess.Popen(['baresip', '-f', '/root/.baresip', '-e', f'/dial sip:{extension}@asterisk',
                          '-t', str(int(seconds))], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
start = time.time()


def show():
    for line in phone.stdout:
        text = line.decode('utf-8', 'replace').strip()
        if any(w in text for w in ('DTMF', 'dtmf', 'established', 'closed', 'terminated')):
            print(f'{time.time() - start:5.1f} s: {text}', flush=True)


threading.Thread(target=show, daemon=True).start()
udp = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
for at, key in sorted(keys):
    time.sleep(max(0, at - (time.time() - start)))
    udp.sendto(key.encode(), ('127.0.0.1', 5555))
    time.sleep(0.15)
    udp.sendto(b'\x04', ('127.0.0.1', 5555))        # baresip: key released
    print(f'{time.time() - start:5.1f} s: pressed {key}', flush=True)
phone.wait()
