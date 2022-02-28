import os.path
import pickle
import socket
import threading
import time

from Actions import Actions
from Client import Client
from HandleClients import HandleClients
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT_TCP = 50000
GATEWAY_PORT_UDP = 60000
MSG_SIZE = 50000
IP = socket.gethostbyname(socket.gethostname())
ADDR_TCP = (IP, GATEWAY_PORT_TCP)
ADDR_UDP = (IP, GATEWAY_PORT_UDP)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR_TCP)
        self.udp_socket = None
        self.window_size: list = []
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
            self.handler.send_all(f"SERVER: {curr_name} has joined the chat!")
            self.handler.send_message("SERVER: connection successful!", conn)
            self.handler.send_message("SERVER: type /commands for more options", conn)
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
                thread = threading.Thread(target=self.handle_client_udp, args=(client,))
                thread.start()

    def handle_client_udp(self, client: Client):
        start_time = time.time()
        self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.udp_socket.bind(ADDR_UDP)
        data, addr = self.udp_socket.recvfrom(MSG_SIZE)
        if not self.check_reliablity(data, addr):
            self.udp_socket.close()
            return
        file_name = self.udp_socket.recvfrom(MSG_SIZE)[0].decode()
        if not self.handler.check_file_name(file_name):
            self.handler.send_message("SERVER: file name not found, try again.", client.client_socket)
            self.udp_socket.sendto("w".encode(), addr)
            self.udp_socket.close()
            return
        else:
            self.udp_socket.sendto("g".encode(), addr)
            file_path = f'ServerFiles/{file_name}'
            packets = self.create_packets_list(file_path)
            cut_list_index = len(packets) / 2
            first_half_packets = packets[:int(cut_list_index)]
            second_half_packets = packets[int(cut_list_index):]
            if not client.received_half_file:
                client.set_received_half_file(True)
                self.handler.send_message("--------downloading file--------", client.client_socket)
                self.send_packets(first_half_packets, addr, int(cut_list_index), False)
                last_byte = self.get_last_file_bytes(file_path, False)
                self.handler.send_message("--------downloaded 50% of the file--------", client.client_socket)
                self.handler.send_message("--------click proceed to download second half--------", client.client_socket)
                self.handler.send_message(f"--------last byte is:{last_byte}--------", client.client_socket)
                print(f"finished downloading first half of file. TOTAL TIME: {time.time() - start_time}")
                self.udp_socket.close()
            else:
                client.set_received_half_file(False)
                self.handler.send_message("--------proceeding to download file--------", client.client_socket)
                self.send_packets(second_half_packets, addr, int(cut_list_index), True)
                last_byte = self.get_last_file_bytes(file_path, True)
                self.handler.send_message("--------FINISHED--------", client.client_socket)
                self.handler.send_message(f"--------last byte is:{last_byte}--------", client.client_socket)
                print(f"finished downloading the file. TOTAL TIME: {time.time() - start_time}")
                self.udp_socket.close()

    def get_last_file_bytes(self, file_path, is_second_part):
        f = open(file_path, 'rb')
        file_size = os.path.getsize(file_path)
        if is_second_part:
            return int.from_bytes(f.read()[-1:], byteorder='little', signed=True)
        else:
            return int.from_bytes(f.read()[int(file_size / 2):int(file_size / 2) + 1], byteorder='little', signed=True)

    def create_packets_list(self, file_path):
        packets = []
        sequence_num = 0
        file_size = os.path.getsize(file_path)
        slice_to_read = 10000
        if file_size < slice_to_read:
            slice_to_read = int(file_size / 5)
        with open(file_path, 'rb', 0) as f:
            while True:
                data = f.read(slice_to_read)
                if not data:
                    break
                else:
                    pkt = [sequence_num, data]
                    pkt = pickle.dumps(pkt)
                    packets.append(pkt)
                    sequence_num += 1
        return packets

    def send_packets(self, packets: list, addr, num, is_second_part):
        num_of_packets = len(packets)
        done = False
        if num_of_packets < 4:
            self.window_size = list(range(0, num_of_packets))
        else:
            self.window_size = [0, 1, 2, 3]
        self.udp_socket.sendto(str(num_of_packets).encode(), addr)
        self.send_window(packets, addr)
        while not done:
            if is_second_part:
                expected_ack = self.window_size[0] + num
            else:
                expected_ack = self.window_size[0]
            ack = int(self.udp_socket.recvfrom(MSG_SIZE)[0].decode())
            if ack == -10:
                done = True
            elif ack == -1:
                print("resending packets")
                self.send_window(packets, addr)
            elif ack == expected_ack:
                print(f"ack received successfully: {ack}")
                next_packet = self.window_size[-1] + 1
                if next_packet < len(packets):
                    self.udp_socket.sendto(packets[next_packet], addr)
                    self.window_size.remove(self.window_size[0])
                    print("sliding window..")
                    print(f"appending next packet : {self.window_size}")
                    self.window_size.append(next_packet)
            else:
                if is_second_part:
                    ack_to_cut = self.window_size.index(ack - num)
                else:
                    ack_to_cut = self.window_size.index(ack)
                print(f"ack received: {ack}")
                resend = self.window_size[:ack_to_cut]
                print(f"resending: {resend}")
                self.resend_packets(packets, resend, addr)
                if ack_to_cut < len(self.window_size) - 1:
                    first_part = self.window_size[(ack_to_cut + 1):]
                    second_part = self.window_size[:ack_to_cut]
                    last_part = first_part[-1] + 1
                    self.window_size = first_part + second_part
                    if last_part < len(packets):
                        self.window_size.append(last_part)
                        self.udp_socket.sendto(packets[last_part], addr)
                    print(f"new window : {self.window_size}")
                else:
                    self.window_size = self.window_size[:ack_to_cut].append(self.window_size[-1] + 1)

    def send_window(self, packets: list, addr):
        for pkt in self.window_size:
            self.udp_socket.sendto(packets[pkt], addr)

    def resend_packets(self, packets: list, resend: list, addr):
        for pkt in resend:
            if pkt < len(packets):
                self.udp_socket.sendto(packets[pkt], addr)

    def check_reliablity(self, data, addr):
        if data.decode() == "ACK":
            self.udp_socket.sendto("SYN".encode(), addr)
            data, _ = self.udp_socket.recvfrom(MSG_SIZE)
            return True
        return False
