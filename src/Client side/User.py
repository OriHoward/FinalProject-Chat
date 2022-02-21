import socket

from Actions import Actions
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT = 50000
PREFIX = '/'


class User:
    def __init__(self, user_name: str = None, ip: str = "127.0.0.1"):
        self.user_name = user_name
        # if ip == "localhost":
        #     self.ip = "127.0.0.1"
        # self.ip = ip
        self.ip = socket.gethostbyname(socket.gethostname())
        self.address = (self.ip, GATEWAY_PORT)
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.is_connected = False

    def connect(self):
        try:
            self.bind_port()
            self.server.connect(self.address)
            SocketHandler.send_msg(self.user_name, self.server)
            print(self.server.getsockname())
            self.is_connected = True
        except:
            print("No connection")

    def disconnect(self):
        SocketHandler.send_enum(Actions.DISCONNECT.value, self.server)
        self.is_connected = False

    def get_users(self):
        SocketHandler.send_enum(Actions.USER_LIST.value, self.server)

    def send_private_msg(self, msg, user_name):
        msg = " ".join(msg)
        SocketHandler.send_enum(Actions.PRIVATE_MSG.value, self.server)
        SocketHandler.send_msg(user_name, self.server)
        SocketHandler.send_msg(msg, self.server)

    def action_received(self, msg: str):
        msg = msg.split(" ")
        match msg[0][1:]:
            case "users" | "getusers":
                self.get_users()
            case "disconnect" | "d":
                self.disconnect()
            case "files" | "getfiles":
                self.get_file_list()
            case "whisper" | "w":
                if len(msg) > 2:
                    self.send_private_msg(msg[2:], msg[1])
            case "commands":
                self.get_commands()

    def send_msg_to_all(self):
        SocketHandler.send_enum(Actions.MESSAGE_ALL.value, self.server)

    def get_file_list(self):
        SocketHandler.send_enum(Actions.FILE_LIST.value, self.server)

    def get_commands(self):
        SocketHandler.send_enum(Actions.COMMANDS.value, self.server)

    def bind_port(self):
        taken_ports = 0
        curr_port = 55000
        is_found = False
        while not is_found:
            try:
                self.server.bind(('', curr_port))
                is_found = True
            except:
                curr_port += 1
                taken_ports += 1

            if taken_ports > 15:
                print("couldn't find a free port within range - chat room is full")
                exit(1)
        return

    def download_file(self):
        pass
