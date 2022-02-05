import socket

from Client import Client
from socketHandler import socketHandler

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, PORT)


class HandleClients:
    def __init__(self):
        self.clients: dict[str, Client] = {}

    def add_client(self, client: Client):
        self.clients[client.user_name] = client

    def get_clients(self):
        return self.clients.keys()

    def send_all(self, msg, sender_client: Client):
        for user_name, client in self.clients:
            if user_name != sender_client.user_name:
                socketHandler.send_msg(msg, client.client_socket)

    def send_to(self, msg, from_client: Client, to_client: Client):
        pass

    def get_client(self, key):
        return self.clients[key]
