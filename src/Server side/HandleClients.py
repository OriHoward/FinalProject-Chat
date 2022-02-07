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

    def get_client(self, key):
        return self.clients[key]

    def send_user_list(self, conn):
        client_list = ""
        for client in self.get_clients():
            client_list += f"{client},"
        client_list = client_list[:len(client_list) - 1]
        SocketHandler.send_msg(client_list, conn)

    def handle_private_msg(self, client: Client):
        dst_client_user_name = SocketHandler.get_msg(client.client_socket)
        dst_client_socket = self.get_client(dst_client_user_name).client_socket
        msg = SocketHandler.get_msg(client.client_socket)
        SocketHandler.send_msg(msg, dst_client_socket)

    def disconnect(self, curr_client):
        curr_client.disconnect()
        self.remove_client(curr_client)
        self.send_all(f"{curr_client.user_name} has disconnected")
