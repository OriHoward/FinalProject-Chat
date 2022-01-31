import socket
import threading

from Client import Client

HEADER = 64


class Server:
    def __init__(self):
        self.clients: dict[Client] = {}
        self.port: int = 55000
        self.ip: str = socket.gethostbyname(socket.gethostname())
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.address: tuple = (self.ip, self.port)

    def bind(self):
        self.server.bind(self.address)

    def handle_client(self, conn, addr):
        print(f"[NEW CONNECTION] {addr} connected")
        connected = True
        while connected:
            msg_length = conn.recv(HEADER).decode('utf-8')
            msg_length = int(msg_length)
            msg = conn.recv(msg_length).decode('utf-8')
            if msg == "DISCONNECT":
                connected = False
            print(f"[{addr}] {msg}")
        conn.close()

    def start(self):
        print("[STARTING] server is starting..")
        self.server.listen()
        while True:
            conn, addr = self.server.accept()
            thread = threading.Thread(target=self.handle_client, args=(conn, addr))
            thread.start()
            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")
