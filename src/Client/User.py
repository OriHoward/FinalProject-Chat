import socket
from struct import unpack

from Actions import Actions
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT_TCP = 50000
GATEWAY_PORT_UDP = 60000
PREFIX = '/'
MSG_SIZE = 4096
DEFAULT_IP = socket.gethostbyname(socket.gethostname())


class User:
    def __init__(self, user_name: str = None, ip: str = DEFAULT_IP):
        self.user_name = user_name
        if ip == "localhost":
            self.ip = DEFAULT_IP
        self.ip = ip
        # self.ip = socket.gethostbyname(socket.gethostname())
        self.address = (self.ip, GATEWAY_PORT_TCP)
        self.udp_address = (self.ip, GATEWAY_PORT_UDP)
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.udp_socket = None
        self.is_connected = False
        self.reuse_socket = False

    def set_username(self, username):
        self.user_name = username

    def set_address(self, ip):
        self.ip = ip

    def connect(self):
        try:
            # self.bind_port()
            self.server.connect(self.address)
            SocketHandler.send_msg(self.user_name, self.server)
            print(self.server.getsockname())
            self.is_connected = True
        except Exception as e:
            print(e)
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
            except Exception as e:
                print(e)
                curr_port += 1
                taken_ports += 1

            if taken_ports > 15:
                print("couldn't find a free port within range - chat room is full")
                exit(1)
        self.reuse_socket = True
        return

    def download_file(self, file_name):
        self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        SocketHandler.send_enum(Actions.OPEN_UDP.value, self.server)
        if self.check_reliablity():
            self.udp_socket.sendto(file_name.encode(), self.udp_address)
            if self.udp_socket.recvfrom(MSG_SIZE)[0].decode() == "w":
                self.close_udp_connection(self.udp_socket)
                return
        else:
            self.close_udp_connection(self.udp_socket)
            return
        self.get_packets()
        ## send to function that takes care of everything here
        print("good")
        self.close_udp_connection(self.udp_socket)

    def get_packets(self):
        pass

    def close_udp_connection(self, udp_socket):
        udp_socket.close()

    def check_reliablity(self):
        self.udp_socket.sendto("ACK".encode(), self.udp_address)
        msg, _ = self.udp_socket.recvfrom(MSG_SIZE)
        if msg.decode() == "SYN":
            self.udp_socket.sendto("ACK".encode(), self.udp_address)
            return True
        else:
            self.udp_socket.close()
            return False

    def calculate_checksum(self, packet):
        total = 0
        num_words = len(packet) // 2
        for chunk in unpack("!%sH" % num_words, packet[0:num_words * 2]):
            total += chunk

        if len(packet) % 2:
            total += packet[-1] << 8

        total = (total >> 16) + (total & 0xffff)
        total += total >> 16
        return ~total + 0x10000 & 0xffff
