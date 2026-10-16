import os
import socket
import threading
import time
from urllib.parse import parse_qsl

# Welcher Content-Type zu welcher Endung gehört
TYPES = {
    '.html': 'text/html; charset=utf-8',
    '.css': 'text/css; charset=utf-8',
    '.js': 'text/javascript; charset=utf-8',
    '.png': 'image/png',
    '.jpg': 'image/jpeg',
    '.txt': 'text/plain; charset=utf-8',
}

PUBLIC_DIR = os.path.realpath(os.path.join(os.path.dirname(__file__), 'public'))

# Der Zähler gehört dem Server, nicht einer Verbindung: Alle Threads
# benutzen ihn, darum zählt immer nur einer auf einmal (Lock).
visits = 0
visits_lock = threading.Lock()


def respond(client, status, content_type, body):
    """Schickt eine vollständige HTTP-Antwort."""
    data = body.encode() if isinstance(body, str) else body
    head = (f'HTTP/1.1 {status}\r\n'
            f'Content-Type: {content_type}\r\n'
            f'Content-Length: {len(data)}\r\n'
            'Connection: close\r\n'
            '\r\n')
    client.sendall(head.encode() + data)


def serve_file(client, path):
    """Eine Datei aus dem Ordner public, aber nie etwas außerhalb davon"""
    if path == '/':
        path = '/index.html'
    file = os.path.realpath(os.path.join(PUBLIC_DIR, '.' + path))
    if file.startswith(PUBLIC_DIR + '/') and os.path.isfile(file):
        content_type = TYPES.get(os.path.splitext(file)[1], 'application/octet-stream')
        with open(file, 'rb') as f:
            respond(client, '200 OK', content_type, f.read())
    else:
        respond(client, '404 Not Found', 'text/plain; charset=utf-8',
                f'Die Seite {path} gibt es nicht.\n')


def handle(client):
    global visits
    with client, client.makefile('rb') as reader:
        request_line = reader.readline().decode()
        method, _, rest = request_line.partition(' ')
        target, _, version = rest.partition(' ')
        print(method, target)
        for line in reader:
            if line == b'\r\n':
                break

        # /hello?name=Ada: der Pfad und die Parameter nach dem Fragezeichen
        path, _, query = target.partition('?')
        params = dict(parse_qsl(query))

        if path == '/time':
            respond(client, '200 OK', 'text/plain; charset=utf-8',
                    f'Es ist {time.strftime("%H:%M:%S")} Uhr.\n')
        elif path == '/hello':
            name = params.get('name', 'Unbekannt')
            respond(client, '200 OK', 'text/plain; charset=utf-8', f'Hallo, {name}!\n')
        elif path == '/visits':
            with visits_lock:
                visits += 1
                count = visits
            respond(client, '200 OK', 'text/plain; charset=utf-8',
                    f'Du bist Besuch Nummer {count}.\n')
        else:
            serve_file(client, path)


server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
