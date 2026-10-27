import html
import socket
import threading
from urllib.parse import parse_qsl


def respond(client, status, content_type, body, extra=None):
    """Schickt eine vollständige HTTP-Antwort, auf Wunsch mit weiteren Headern."""
    data = body.encode() if isinstance(body, str) else body
    head = (f'HTTP/1.1 {status}\r\n'
            f'Content-Type: {content_type}\r\n'
            f'Content-Length: {len(data)}\r\n')
    for name, value in (extra or {}).items():
        head += f'{name}: {value}\r\n'
    head += 'Connection: close\r\n\r\n'
    client.sendall(head.encode() + data)


def h(text):
    """Text, der in HTML sicher ist: aus < wird &lt; und so weiter"""
    return html.escape(str(text if text is not None else ''))


def headers_page(headers):
    """Die Seite unter /: alle Header, die der Browser geschickt hat"""
    rows = '\n'.join(f'<tr><th>{h(name)}</th><td>{h(value)}</td></tr>' for name, value in headers.items())
    return ("<!DOCTYPE html><meta charset='utf-8'><title>Was ich über dich weiß</title>\n"
            f'<h1>Was dieser Server über dich weiß</h1>\n<table>\n{rows}\n</table>\n')


def handle(client):
    with client, client.makefile('rb') as reader:
        request_line = reader.readline().decode()
        method, _, rest = request_line.partition(' ')
        target, _, version = rest.partition(' ')
        # Die Header in ein dict: »Name: Wert«
        headers = {}
        for line in reader:
            if line == b'\r\n':
                break
            name, _, value = line.decode().partition(':')
            headers[name.strip().lower()] = value.strip()
        path, _, query = target.partition('?')
        params = dict(parse_qsl(query))
        print(method, target)
        if 'referer' in headers:
            print('  Referer:', headers['referer'])

        if path == '/':
            respond(client, '200 OK', 'text/html; charset=utf-8', headers_page(headers))
        else:
            respond(client, '404 Not Found', 'text/plain; charset=utf-8', f'Die Seite {path} gibt es nicht.\n')


server = socket.create_server(('', 8000))
print('Der Tracker wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
