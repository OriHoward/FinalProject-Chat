import socket
import threading
from HandleClients import HandleClients
from Client import Client

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, PORT)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR)
        self.handler = HandleClients()

    def handle_client(self, conn, addr):
        print(f"[NEW CONNECTION] {addr} connected.")
        connected = True
        while connected:
            msg = self.get_msg(conn)
            #todo - ENUM CLASS for action numbers, switch case on msg - sending to handler functions according to the number
            if msg == "DISCONNECT":
                connected = False
            print(f"[{addr}] {msg}")
        conn.close()

    def start(self):
        print("[LISTENING] server is now listening..")
        self.server.listen()
        while True:
            conn, addr = self.server.accept()
            curr_name = self.get_msg(conn)
            self.handler.add_client(Client(conn, curr_name))
            thread = threading.Thread(target=self.handle_client, args=(conn, addr))
            thread.start()
            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")

    def get_msg(self, conn):
        msg_length = conn.recv(HEADER).decode(FORMAT)
        if msg_length:
            msg_length = int(msg_length)
            return conn.recv(msg_length).decode(FORMAT)

    def send_msg(self, msg):
        message = msg.encode(FORMAT)
        msg_length = len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER - len(send_length))  # adding padding to fill the 64 bytes
        self.server.send(send_length)
        self.server.send(message)
