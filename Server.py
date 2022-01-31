import socket
import threading

class Server:
    def __init__(self):
        self.clients = {}
        self.port = 55000
        self.ip = socket.gethostbyname(socket.gethostname())






