import socket

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
PREFIX = '/'


class User:
    def __init__(self, user_name: str = ""):
        self.user_name = user_name
        self.ip = socket.gethostbyname(socket.gethostname())
        self.address = (self.ip, PORT)
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def connect(self):
        self.server.connect(self.address)
        self.send_msg(self.user_name)


    def disconnect(self):
        pass

    def get_users(self):
        self.server.send(bytes(200))
        msg = self.get_msg()

    #
    # self.client.recv(SIZE OF LIST)
    # self.client.recv(THE LIST ITSELF)

    def get_msg(self):
        msg_length = self.server.recv(HEADER).decode(FORMAT)
        if msg_length:
            msg_length = int(msg_length)
            return self.server.recv(msg_length).decode(FORMAT)

    def send_msg(self, msg):
        message = msg.encode(FORMAT)
        msg_length = len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER - len(send_length))  # adding padding to fill the 64 bytes
        self.server.send(send_length)
        self.server.send(message)

    def action_received(self, msg: str):
        if msg[0] == PREFIX:
            match msg[1:]:
                case "get_users":
                    self.get_users()
                case "get_file_list":
                    pass
        else:
            self.send_msg_to_all()

    def send_msg_to_all(self):
        pass
