from pathlib import Path
from websockets.datastructures import Headers
from websockets.http11 import Response
from websockets.sync.server import serve

PAGE = Path(__file__).with_name('index.html')


# Jede Anfrage kommt zuerst hier an. Anfragen an /ws gehen weiter zum
# Handshake; alle anderen bekommen die Webseite.
def page(connection, request):
    if request.path == '/ws':
        return None
    body = PAGE.read_bytes()
    headers = Headers({'Content-Type': 'text/html; charset=utf-8',
                       'Content-Length': str(len(body))})
    return Response(200, 'OK', headers, body)


# Läuft für jede WebSocket-Verbindung, solange sie offen ist
def handle(ws):
    # Jede Nachricht geht mit »Echo:« davor zurück
    for message in ws:
        print(f'Empfangen: {message}')
        ws.send(f'Echo: {message}')


with serve(handle, '0.0.0.0', 8000, process_request=page) as server:
    print('Der Server wartet auf Port 8000 ...')
    server.serve_forever()
