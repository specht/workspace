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

# Die Umfrage: was im Formular steht, wie es auf der Seite heißt, und die
# Stimmen dazu, ebenfalls geschützt durch einen Lock
OPTIONS = {'pizza': 'Pizza', 'pasta': 'Nudeln', 'curry': 'Curry', 'salad': 'Salat'}
votes = {key: 0 for key in OPTIONS}
votes_lock = threading.Lock()


def respond(client, status, content_type, body):
    """Schickt eine vollständige HTTP-Antwort."""
    data = body.encode() if isinstance(body, str) else body
    head = (f'HTTP/1.1 {status}\r\n'
            f'Content-Type: {content_type}\r\n'
            f'Content-Length: {len(data)}\r\n'
            'Connection: close\r\n'
            '\r\n')
    client.sendall(head.encode() + data)


def redirect(client, location):
    """Schickt den Browser weiter zu einer anderen Adresse: »Sieh dort nach«"""
    client.sendall((f'HTTP/1.1 303 See Other\r\n'
                    f'Location: {location}\r\n'
                    'Content-Length: 0\r\n'
                    'Connection: close\r\n'
                    '\r\n').encode())


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
        # Die Header in ein dict: »Name: Wert«
        headers = {}
        for line in reader:
            if line == b'\r\n':
                break
            name, _, value = line.decode().partition(':')
            headers[name.strip().lower()] = value.strip()

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
        elif path == '/vote':
            if method != 'POST':
                respond(client, '405 Method Not Allowed', 'text/plain; charset=utf-8',
                        'Abstimmen geht nur mit POST.\n')
                return
            # Nach der leeren Zeile kommt der Inhalt, genau Content-Length Bytes
            body = reader.read(int(headers.get('content-length', '0'))).decode()
            choice = dict(parse_qsl(body)).get('choice')
            if choice in OPTIONS:
                with votes_lock:
                    votes[choice] += 1
                redirect(client, '/results')
            else:
                respond(client, '400 Bad Request', 'text/plain; charset=utf-8',
                        'Diese Antwort gibt es nicht.\n')
        elif path == '/results':
            # Ein Balken für jede Antwort; <progress> zeichnet ihn ohne CSS
            with votes_lock:
                most = max(max(votes.values()), 1)
                rows = '\n'.join(
                    f"<tr><td>{label}</td><td><progress value='{votes[key]}' max='{most}'></progress></td>"
                    f"<td>{votes[key]}</td></tr>"
                    for key, label in OPTIONS.items())
            respond(client, '200 OK', 'text/html; charset=utf-8',
                    "<!DOCTYPE html><meta charset='utf-8'><link rel='stylesheet' href='style.css'>"
                    f"<h1>Ergebnis</h1><table>{rows}</table><p><a href='poll.html'>Noch einmal abstimmen</a></p>\n")
        else:
            serve_file(client, path)


server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
