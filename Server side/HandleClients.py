import socket
from http import client
from Client import Client

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


    # def get_users(self):
    #     print("connected users:")
    #     for client in self.clients.values():
    #         print(f"name: {client.name}, id: {client._id}")

    #
    # def send_message_to_all(self, msg):
    #     pass
    #
    # def disconnect(self, user: User):
    #     self.send_msg_to_server(user, "DISCONNECT")

    # def send_msg_to_server(self, user: User, msg):
    #     message = msg.encode(FORMAT)
    #     msg_length = len(message)
    #     send_length = str(msg_length).encode(FORMAT)
    #     send_length += b' ' * (HEADER - len(send_length))  # adding padding to fill the 64 bytes
    #     user.client.send(send_length)
    #     user.client.send(message)
