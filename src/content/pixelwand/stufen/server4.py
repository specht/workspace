import json
import queue
import threading
from pathlib import Path
from websockets.datastructures import Headers
from websockets.exceptions import ConnectionClosed
from websockets.http11 import Response
from websockets.sync.server import serve

PAGE = Path(__file__).with_name('index.html')

# Das Bild: 64 × 40 Pixel, jedes eine Farbnummer von 0 bis 15
WIDTH = 64
HEIGHT = 40
pixels = [0] * (WIDTH * HEIGHT)

# Alle offenen Verbindungen
clients = set()

# Wie viele Pixel schon gemalt wurden
painted = 0


# Das Bild in eine Datei schreiben, damit es einen Neustart übersteht
def save_picture():
    Path('picture.json').write_text(json.dumps(pixels))


def broadcast(message):
    text = json.dumps(message)
    for client in list(clients):
        try:
            client.send(text)
        except ConnectionClosed:
            pass


def work():
    global painted
    while True:
        text = messages.get()       # wartet, bis eine Nachricht da ist

        # Jeder kann alles schicken: nur glauben, was passt
        try:
            message = json.loads(text)
        except ValueError:
            continue
        if not isinstance(message, dict):
            continue
        x, y, color = message.get('x'), message.get('y'), message.get('color')
        if not all(type(value) is int for value in (x, y, color)):
            continue
        if not (0 <= x < WIDTH and 0 <= y < HEIGHT and 0 <= color <= 15):
            continue

        pixels[y * WIDTH + x] = color
        count = painted
        save_picture()              # dauert einen Moment
        painted = count + 1
        print(f'Pixel Nr. {painted}')
        broadcast({'type': 'pixel', 'x': x, 'y': y, 'color': color})


# Vier Threads arbeiten die Nachrichten ab, während die Verbindung schon die
# nächste annimmt. Mehr als ein paar sollten es nicht sein: Jeder Thread
# kostet Speicher, und ein Workspace darf nicht beliebig viele starten.
messages = queue.Queue()
for _ in range(4):
    threading.Thread(target=work, daemon=True).start()


def page(connection, request):
    if request.path == '/ws':
        return None
    body = PAGE.read_bytes()
    headers = Headers({'Content-Type': 'text/html; charset=utf-8',
                       'Content-Length': str(len(body))})
    return Response(200, 'OK', headers, body)


def handle(ws):
    # Wer neu ist, bekommt das ganze Bild, danach nur noch einzelne Pixel
    clients.add(ws)
    ws.send(json.dumps({'type': 'picture', 'width': WIDTH,
                        'height': HEIGHT, 'pixels': pixels}))
    broadcast({'type': 'people', 'count': len(clients)})
    try:
        # Jede Nachricht kommt in die Warteschlange; die Verbindung ist sofort frei
        for text in ws:
            messages.put(text)
    finally:
        clients.discard(ws)
        broadcast({'type': 'people', 'count': len(clients)})


with serve(handle, '0.0.0.0', 8000, process_request=page) as server:
    print('Der Server wartet auf Port 8000 ...')
    server.serve_forever()
