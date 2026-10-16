import os
import socket
import threading
import time

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
    with client, client.makefile('rb') as reader:
        request_line = reader.readline().decode()
        method, _, rest = request_line.partition(' ')
        path, _, version = rest.partition(' ')
        print(method, path)
        for line in reader:
            if line == b'\r\n':
                break

        if path == '/time':
            respond(client, '200 OK', 'text/plain; charset=utf-8',
                    f'Es ist {time.strftime("%H:%M:%S")} Uhr.\n')
        else:
            serve_file(client, path)


server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
