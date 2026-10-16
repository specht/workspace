from pathlib import Path
from websockets.datastructures import Headers
from websockets.exceptions import ConnectionClosed
from websockets.http11 import Response
from websockets.sync.server import serve

PAGE = Path(__file__).with_name('index.html')

# Alle offenen Verbindungen. Jede Verbindung hat ihren eigenen Thread, alle
# benutzen dieselbe Menge.
clients = set()


def broadcast(text):
    # Über eine Kopie: Kommt während der Schleife jemand dazu, ändert sich
    # die Menge, und die Schleife bräche mit einem Fehler ab.
    for client in list(clients):
        try:
            client.send(text)
        except ConnectionClosed:
            pass    # gerade gegangen; das Aufräumen erledigt ihr eigener Thread


def page(connection, request):
    if request.path == '/ws':
        return None
    body = PAGE.read_bytes()
    headers = Headers({'Content-Type': 'text/html; charset=utf-8',
                       'Content-Length': str(len(body))})
    return Response(200, 'OK', headers, body)


def handle(ws):
    clients.add(ws)
    broadcast(f'Jemand ist dazugekommen. Jetzt sind wir {len(clients)}.')
    try:
        # Was kommt, geht an alle
        for message in ws:
            print(f'Empfangen: {message}')
            broadcast(message)
    finally:
        # Die Verbindung ist zu, weil jemand den Tab geschlossen hat
        clients.discard(ws)
        broadcast(f'Jemand ist gegangen. Jetzt sind wir {len(clients)}.')


with serve(handle, '0.0.0.0', 8000, process_request=page) as server:
    print('Der Server wartet auf Port 8000 ...')
    server.serve_forever()
