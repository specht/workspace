import socket
import threading


def handle(client):
    with client, client.makefile('rb') as reader:
        for line in reader:
            if line == b'\r\n':
                break
            print(line.decode(), end='')

        body = 'Hallo aus meinem eigenen Server!\n'.encode()
        client.sendall(b'HTTP/1.1 200 OK\r\n')
        client.sendall(b'Content-Type: text/plain; charset=utf-8\r\n')
        client.sendall(f'Content-Length: {len(body)}\r\n'.encode())
        client.sendall(b'Connection: close\r\n')
        client.sendall(b'\r\n')
        client.sendall(body)


server = socket.create_server(('', 8000))
print('Der Server wartet auf Port 8000 ...')

while True:
    client, address = server.accept()
    # Jede Verbindung bekommt einen eigenen Thread: So wartet der Server nicht
    # auf einen Besucher, während ein anderer schon etwas will.
    threading.Thread(target=handle, args=(client,), daemon=True).start()
