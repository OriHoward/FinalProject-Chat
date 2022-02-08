import socket
from Actions import Actions
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
HEADER = 64
GATEWAY_PORT = 50000
PREFIX = '/'


# UPPER_BOUND = 55015
# LOWER_BOUND = 55000
# ports = Ports(LOWER_BOUND, UPPER_BOUND)


class User:
    def __init__(self, user_name: str = None):
        self.user_name = user_name
        self.ip = socket.gethostbyname(socket.gethostname())
        self.address = (self.ip, GATEWAY_PORT)
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # self.port = ports.check_for_ports()
        # self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        # self.server.bind(('', self.port))
        # print(self.port)
        self.is_connected = False

    def connect(self):
        try:
            self.server.connect(self.address)
            SocketHandler.send_msg(self.user_name, self.server)
            self.is_connected = True
        except:
            print("No connection")

    def disconnect(self):
        SocketHandler.send_enum(Actions.DISCONNECT.value, self.server)
        self.is_connected = False
        # ports.ports[self.port - LOWER_BOUND] = False

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

    def send_msg_to_all(self):
        SocketHandler.send_enum(Actions.MESSAGE_ALL.value, self.server)

    def get_file_list(self):
        pass
