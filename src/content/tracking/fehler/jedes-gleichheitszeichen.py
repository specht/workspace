import base64
import html
import os
import secrets
import socket
import threading
import time
from urllib.parse import parse_qsl

# Ein Bild, das niemand sieht: ein einziges durchsichtiges Pixel im Format GIF
PIXEL = base64.b64decode('R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7')

# Hier schreibt der Tracker jeden Besuch auf, eine Zeile pro Besuch
LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'visits.log')
log_lock = threading.Lock()


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


# Ein wenig CSS, damit die Seiten lesbar sind
STYLE = ('<style>body { font-family: system-ui, sans-serif; margin: 2em; }\n'
         'table { border-collapse: collapse; }\n'
         'th, td { padding: 0.3em 0.8em; border-bottom: 1px solid #ddd; text-align: left; vertical-align: top; }\n'
         'th { color: #666; font-weight: normal; }\n'
         'td { word-break: break-all; }</style>\n')


def headers_page(headers):
    """Die Seite unter /: alle Header, die der Browser geschickt hat"""
    rows = '\n'.join(f'<tr><th>{h(name)}</th><td>{h(value)}</td></tr>' for name, value in headers.items())
    return ("<!DOCTYPE html><meta charset='utf-8'><title>Was ich über dich weiß</title>\n" + STYLE +
            f'<h1>Was dieser Server über dich weiß</h1>\n<table>\n{rows}\n</table>\n')


def log_visit(visitor, page, keywords):
    """Eine Zeile im Protokoll: wann, wer, welche Seite, worum es dort geht"""
    line = '\t'.join([time.strftime('%Y-%m-%d %H:%M:%S'), visitor, page or '', keywords or ''])
    with log_lock:
        with open(LOG, 'a') as f:
            f.write(line + '\n')
    print('  Besuch:', line)


def parse_cookies(header):
    """Aus dem Header Cookie (»visitor=3f9a1c2b; theme=dark«) ein dict"""
    cookies = {}
    for pair in header.split(';'):
        name, value = pair.strip().split('=')
        if name:
            cookies[name] = value
    return cookies


def visits_of(lines, visitor):
    """Die Besuche einer Kennung im Protokoll, als Paare aus Seite und Stichwörtern"""
    visits = []
    for line in lines:
        fields = line.split('\t')
        if len(fields) == 4 and fields[1] == visitor:
            visits.append([fields[2], fields[3]])
    return visits


def count_keywords(texts):
    """Wie oft jedes Stichwort vorkommt; jeder Text sind Stichwörter mit Komma"""
    counts = {}
    for text in texts:
        for keyword in (text or '').split(','):
            keyword = keyword.strip()
            if keyword:
                counts[keyword] = counts.get(keyword, 0) + 1
    return counts


def profile_page(visitor, visits, counts, headers):
    """Die Seite unter /profil: was der Tracker über diese Kennung gesammelt hat"""
    pages = '\n'.join(f'<li>{h(page)}</li>' for page, keywords in visits)
    top = sorted(counts.items(), key=lambda pair: -pair[1])[:5]
    interests = '\n'.join(f'<li>{h(keyword)} ({count})</li>' for keyword, count in top)
    return ("<!DOCTYPE html><meta charset='utf-8'><title>Dein Profil</title>\n" + STYLE +
            '<h1>Dein Profil</h1>\n'
            f"<p>Deine Kennung: {h(visitor or 'keine')}</p>\n"
            f"<p>Dein Browser: {h(headers.get('user-agent'))}<br>Deine Sprache: {h(headers.get('accept-language'))}</p>\n"
            f'<h2>Seiten, auf denen du warst</h2>\n<ol>\n{pages}\n</ol>\n'
            f'<h2>Was dich vermutlich interessiert</h2>\n<ol>\n{interests}\n</ol>\n')


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
        elif path == '/pixel.gif':
            visitor = parse_cookies(headers.get('cookie', '')).get('visitor')
            # no-store: Der Browser soll das Bild jedes Mal neu holen
            extra = {'Cache-Control': 'no-store'}
            if not visitor:
                visitor = secrets.token_hex(4)
                # Ein Jahr lang schickt der Browser die Kennung bei jeder Anfrage mit
                extra['Set-Cookie'] = f'visitor={visitor}; Max-Age=31536000; Path=/'
            log_visit(visitor, params.get('page'), params.get('keywords'))
            respond(client, '200 OK', 'image/gif', PIXEL, extra)
        elif path == '/profil':
            visitor = parse_cookies(headers.get('cookie', '')).get('visitor')
            lines = []
            if os.path.exists(LOG):
                with open(LOG) as f:
                    lines = f.read().splitlines()
            visits = visits_of(lines, visitor)
            counts = count_keywords([keywords for page, keywords in visits])
            respond(client, '200 OK', 'text/html; charset=utf-8', profile_page(visitor, visits, counts, headers))
        else:
            respond(client, '404 Not Found', 'text/plain; charset=utf-8', f'Die Seite {path} gibt es nicht.\n')


server = socket.create_server(('', 8000))
print('Der Tracker wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    threading.Thread(target=handle, args=(client,), daemon=True).start()
