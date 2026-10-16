import socket
import threading
import time


def respond(client, status, content_type, body):
    """Schickt eine vollständige HTTP-Antwort."""
    data = body.encode() if isinstance(body, str) else body
    head = (f'HTTP/1.1 {status}\r\n'
            f'Content-Type: {content_type}\r\n'
            f'Content-Length: {len(data)}\r\n'
            'Connection: close\r\n'
            '\r\n')
    client.sendall(head.encode() + data)


def handle(client):
    with client, client.makefile('rb') as reader:
        # Die erste Zeile: Methode, Pfad, Version
        request_line = reader.readline().decode()
        method, _, rest = request_line.partition(' ')
        path, _, version = rest.partition(' ')
        print(method, path)
        # Die übrigen Zeilen (Header) brauchen wir noch nicht
        for line in reader:
            if line == b'\r\n':
                break

        if path == '/':
            respond(client, '200 OK', 'text/html; charset=utf-8',
                    '<h1>Willkommen!</h1><p>Das ist mein eigener Webserver.</p>')
        elif path == '/time':
            respond(client, '200 OK', 'text/plain; charset=utf-8',
                    f'Es ist {time.strftime("%H:%M:%S")} Uhr.\n')
        else:
            respond(client, '404 Not Found', 'text/plain; charset=utf-8',
                    f'Die Seite {path} gibt es nicht.\n')


server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
