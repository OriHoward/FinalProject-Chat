import socket

from Client import Client
from SocketHandler import SocketHandler

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

    def remove_client(self, client: Client):
        self.clients.pop(client.user_name, None)

    def get_clients_list(self):
        users: list = []
        for client in self.clients.keys():
            users.append(client)
        return users

    def send_all(self, msg):
        for client in self.clients.values():
            SocketHandler.send_msg(msg, client.client_socket)

    def send_to(self, msg, from_client: Client, to_client: Client):
        pass

    def get_client(self, key):
        return self.clients[key]
