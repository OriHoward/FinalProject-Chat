import socket
import threading
from time import sleep
from struct import unpack
import pickle
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
            self.udp_socket.sendto(file_name.encode(),addr)
            self.udp_socket.close()

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
                    pkt = [sequence_num, checksum, data]
                    pkt = pickle.dumps(pkt)
                    packets.append(pkt)
                    sequence_num += 1
        return packets

    def send_packets(self, packets: list, addr):
        window_size = 4
        packets_not_sent: list = list(range(0, len(packets)))
        expected_acks: dict = {}
        acks_sent: dict = {}
        print("sending number of packets..")
        self.udp_socket.sendto(str(len(packets)).encode(), addr)
        print("sent successfully")
        while len(packets_not_sent) > 0:
            expected_acks.clear()
            sleep(0.2)
            for pkt in range(window_size):
                if pkt in packets_not_sent:
                    print(f"sending packet number: {pkt}")
                    self.udp_socket.sendto(packets[pkt], addr)
                    ack = self.udp_socket.recvfrom(MSG_SIZE)[0].decode()
                    print("received response from client")
                    ack = ack.split(',')
                    print(f"{ack[1]}, {ack[0]}")
                    acks_sent[int(ack[1])] = ack[0]
                    expected_acks[pkt] = "ACK"
                    print("-------------")
            arrived = self.check_lost_pkts(acks_sent, expected_acks, packets_not_sent)
            print(f"arrived {arrived} packets")
            window_size += arrived

    def check_lost_pkts(self, aks_sent: dict, exepcted_akcs: dict, packets_not_sent: list):
        arrived = 0
        for key in exepcted_akcs.keys():
            print(key)
            if aks_sent[key] == exepcted_akcs[key]:
                print(f"packet number {key} sent successfully, removing from list..")
                packets_not_sent.remove(key)
                arrived += 1

        return arrived

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
