import socket

from Actions import Actions
from socketHandler import socketHandler

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
PREFIX = '/'


class User:
    def __init__(self, user_name: str = None):
        self.user_name = user_name
        self.ip = socket.gethostbyname(socket.gethostname())
        self.address = (self.ip, PORT)
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def connect(self):
        self.server.connect(self.address)
        socketHandler.send_msg(self.user_name, self.server)

    def disconnect(self):
        self.server.send(Actions.DISCONNECT.value)

    def get_users(self):
        self.server.send(Actions.USER_LIST.value)
        # clients_list: str = socketHandler.get_msg(self.server)
        # print(clients_list)

    # def send_all(self, msg):
    #     self.server.send(Actions.MESSAGE_ALL.value)
    #     # return socketHandler.send_msg(msg, self.server)

    def send_private_msg(self, msg, user_name: str):
        self.server.send(Actions.PRIVATE_MSG.value)
        socketHandler.send_msg(user_name, self.server)
        socketHandler.send_msg(msg, self.server)

    def action_received(self, msg: str):
        match msg[1:]:
            case "get users":
                self.get_users()
            case "disconnect":
                self.disconnect()
            case "get file list":
                pass

    def send_msg_to_all(self):
        self.server.send(Actions.MESSAGE_ALL.value)
