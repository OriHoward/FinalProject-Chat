import socket

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, PORT)


class Client:
    def __init__(self, name: str = "", _id: str = ""):
        self.name = name
        self._id = _id
        self.client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def connect(self):
        self.client.connect(ADDR)

    def disconnect(self):
        self.send_msg_to_server("DISCONNECT")

    def send_msg_to_server(self, msg):
        message = msg.encode(FORMAT)
        msg_length =len(message)
        send_length = str(msg_length).encode(FORMAT)
        send_length += b' ' * (HEADER-len(send_length)) # adding padding to fill the 64 bytes
        self.client.send(send_length)
        self.client.send(message)

    def send_message_to(self, msg, client):
        pass

    def send_message_to_all(self, msg):
        pass

    def get_users(self):
        pass

    def get_list_file(self):
        pass

    def request_file(self):
        pass

    def download_file(self):
        pass
