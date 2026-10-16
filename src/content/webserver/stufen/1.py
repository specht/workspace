import socket

server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()   # wartet, bis jemand eine Verbindung aufbaut
    print('--- Eine neue Verbindung')
    with client, client.makefile('rb') as reader:
        # Die Anfrage zeilenweise lesen, bis zur ersten leeren Zeile
        for line in reader:
            if line == b'\r\n':
                break
            print(line.decode(), end='')
