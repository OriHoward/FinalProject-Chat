import socket
import threading
from struct import unpack

from Actions import Actions
from Client import Client
from HandleClients import HandleClients
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT_TCP = 50000
GATEWAY_PORT_UDP = 60000
MSG_SIZE = 4096
IP = socket.gethostbyname(socket.gethostname())
ADDR_TCP = (IP, GATEWAY_PORT_TCP)
ADDR_UDP = (IP, GATEWAY_PORT_UDP)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR_TCP)
        self.udp_socket = None
        self.handler = HandleClients()

    def handle_client(self, conn, addr, curr_client: Client):
        print(f"[NEW CONNECTION] {addr} connected.")
        while curr_client.is_connected:
            action_msg = SocketHandler.get_enum(conn)
            if action_msg:
                self.handle_msg(action_msg, curr_client)
        conn.close()

    def start(self):
        print("[LISTENING] server is now listening..")
        self.server.listen()
        while True:
            conn, addr = self.server.accept()
            curr_name = SocketHandler.get_msg(conn)
            is_free = self.handler.is_user_available(curr_name)
            SocketHandler.send_enum(Actions.TRUE.value if is_free else Actions.FALSE.value, conn)
            if not is_free:
                conn.close()
                continue
            self.handler.send_all(f"{curr_name} has joined the chat!")
            SocketHandler.send_msg("connection successful!", conn)
            SocketHandler.send_msg("type /commands for more options", conn)
            new_client = Client(conn, curr_name, addr)
            self.handler.add_client(new_client)
            tcp_thread = threading.Thread(target=self.handle_client, args=(conn, addr, new_client), daemon=True)
            tcp_thread.start()
            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")

    def handle_msg(self, msg, client):
        match msg:
            case Actions.USER_LIST.value:
                self.handler.send_user_list(client.client_socket)
            case Actions.PRIVATE_MSG.value:
                self.handler.handle_private_msg(client)
            case Actions.DISCONNECT.value:
                self.handler.disconnect(client)
            case Actions.MESSAGE_ALL.value:
                msg_to_distribute = SocketHandler.get_msg(client.client_socket)
                self.handler.send_all(msg_to_distribute)
            case Actions.FILE_LIST.value:
                self.handler.send_file_list(client)
            case Actions.COMMANDS.value:
                self.handler.send_commands_list(client)
            case Actions.OPEN_UDP.value:
                self.handle_client_udp(client)

    def handle_client_udp(self, client: Client):
        self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.udp_socket.bind(ADDR_UDP)
        data, addr = self.udp_socket.recvfrom(MSG_SIZE)
        if not self.check_reliablity(data, addr):
            self.udp_socket.close()
            return
        file_name = self.udp_socket.recvfrom(MSG_SIZE)[0].decode()
        if not self.handler.check_file_name(file_name):
            SocketHandler.send_msg("file name not found, try again.", client.client_socket)
            self.udp_socket.sendto("w".encode(), addr)
            self.udp_socket.close()
            return
        else:
            self.udp_socket.sendto("g".encode(), addr)
            file_path = f'ServerFiles/{file_name}'
            packets = self.create_packets_list(file_path)
            self.send_packets(packets, addr)
            # for p in packets:
            #     print(p)
            # self.udp_socket.close()

    def create_packets_list(self, file_path):
        packets = []
        sequence_num = 0
        with open(file_path, 'rb', 0) as f:
            while True:
                data = f.read(50)
                if not data:
                    break
                else:
                    checksum = self.calculate_checksum(data)
                    packets.append(f'{sequence_num},{checksum},{data}')
                    sequence_num += 1
        return packets

    def send_packets(self, packets: list, addr):
        window_size = 4
        packets_not_sent: list = list(range(0, len(packets)))
        expected_acks: list = []
        acks_sent = []
        sent_all = False
        while sent_all:
            start_window_size = 0
            print("sending packet size to client..")
            self.udp_socket.sendto(str(len(packets)).encode(), addr)
            end_window_size = start_window_size + window_size
            while end_window_size < len(packets):
                expected_acks.clear()
                for pkt in range(window_size):
                    if pkt in packets_not_sent:
                        self.udp_socket.sendto(packets[pkt], addr)
                        ack = self.udp_socket.recvfrom(MSG_SIZE)
                        acks_sent.append(ack)
                        expected_acks.append(f"ACK {pkt}")
                ## compare expected acks vs acks and remove from sent

        # self.udp_socket.close()

    def check_reliablity(self, data, addr):
        if data.decode() == "ACK":
            self.udp_socket.sendto("SYN".encode(), addr)
            data, _ = self.udp_socket.recvfrom(MSG_SIZE)
            return True
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

    # todo thread to disconnect and close all ports from console
