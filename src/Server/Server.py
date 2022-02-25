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
            SocketHandler.send_msg("SERVER: connection successful!", conn)
            SocketHandler.send_msg("SERVER: type /commands for more options", conn)
            new_client = Client(conn, curr_name, addr)
            self.handler.add_client(new_client)
            tcp_thread = threading.Thread(target=self.handle_client, args=(conn, addr, new_client), daemon=True)
            # udp_thread = threading.Thread(target=self.handle_client_udp, args=(new_client,))
            tcp_thread.start()
            # udp_thread.start()
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
                # self.handle_client_udp(client)

    def handle_client_udp(self, client: Client):
        self.udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.udp_socket.bind(ADDR_UDP)
        data, addr = self.udp_socket.recvfrom(MSG_SIZE)
        if not self.check_reliablity(data, addr):
            self.udp_socket.close()
            return
        file_name = self.udp_socket.recvfrom(MSG_SIZE)[0].decode()
        if not self.handler.check_file_name(file_name):
            SocketHandler.send_msg("SERVER: file name not found, try again.", client.client_socket)
            self.udp_socket.sendto("w".encode(), addr)
            self.udp_socket.close()
            return
        else:
            self.udp_socket.sendto("g".encode(), addr)
            file_path = f'ServerFiles/{file_name}'
            packets = self.create_packets_list(file_path)
            self.send_packets(packets, addr)
            print("finished")
            # self.udp_socket.sendto(file_name.encode(), addr)
            print("sent")
            self.udp_socket.close()

    def create_packets_list(self, file_path):
        packets = []
        sequence_num = 0
        with open(file_path, 'rb', 0) as f:
            while True:
                data = f.read(50000)
                if not data:
                    break
                else:
                    pkt = [sequence_num, data]
                    pkt = pickle.dumps(pkt)
                    packets.append(pkt)
                    sequence_num += 1
        return packets

    def send_packets(self, packets: list, addr):
        if len(packets) < 4:
            self.window_size = list(range(0, len(packets)))
        else:
            self.window_size = [0, 1, 2, 3]
        packets_received = 0
        num_of_packets = len(packets)
        self.udp_socket.sendto(str(num_of_packets).encode(), addr)
        for pkt in self.window_size:
            self.udp_socket.sendto(packets[pkt], addr)
        while packets_received < num_of_packets:
            expected_ack = self.window_size[0]
            ack = int(self.udp_socket.recvfrom(MSG_SIZE)[0].decode())
            packets_received += 1
            print(packets_received)
            if ack == expected_ack:
                print(f"ack received successfully: {ack}")
                next_packet = self.window_size[-1] + 1
                if next_packet < len(packets):
                    self.udp_socket.sendto(packets[next_packet], addr)
                    self.window_size.remove(ack)
                    print(f"removed ack number: {self.window_size}")
                    self.window_size.append(next_packet)
                    print(f"appended next packet : {self.window_size}")
            else:
                ack_to_cut = self.window_size.index(ack)
                print(f"ack received: {ack}")
                resend = self.window_size[:ack_to_cut]
                print(f"resending: {resend}")
                self.resend_packets(packets, resend, addr)
                if ack_to_cut < len(self.window_size) - 1:
                    first_part = self.window_size[(ack_to_cut + 1):]
                    # print(f"first part: {first_part}")
                    second_part = self.window_size[:ack_to_cut]
                    # print(f"second part: {second_part}")
                    last_part = first_part[-1] + 1
                    # print(f"last part: {last_part}")
                    self.window_size = first_part + second_part
                    self.window_size.append(last_part)
                    if last_part < len(packets):
                        self.udp_socket.sendto(packets[last_part], addr)
                    print(f"new window : {self.window_size}")
                else:
                    self.window_size = self.window_size[:ack_to_cut].append(self.window_size[-1] + 1)

    def resend_packets(self, packets: list, resend: list, addr):
        for pkt in resend:
            if pkt < len(packets):
                self.udp_socket.sendto(packets[pkt], addr)

    # example 1: [0,1,2,3] -> rec 0 -> send 4 -> [1,2,3,4]
    # example 2: [0,1,2,3] -> rec 1 -> send 0 -> [0,1,2,3]
    #            [0 1 2 3] -> rec 3 -> send 0, 1 ,2 -> rec 2 -> send 0,1 -> rec 1 -> send 0
    #
    #            [0 1 2 3] -> rec 3 -> send 0, 1 ,2, 4 -> rec 2 -> send 0,1 -> rec 1 -> send 0
    # def send_packets(self, packets: list, addr):
    #     self.window_size = 4
    #     expected_ack = 0
    #     last_sent = None
    #     num_of_packets = len(packets)
    #     packets_received = 0
    #     self.udp_socket.sendto(str(num_of_packets).encode(), addr)
    #     expected = [0 for _ in range(window_size)]
    #     for i, pkt_index in enumerate(range(window_size)):
    #         self.udp_socket.sendto(packets[pkt_index], addr)
    #         expected[i] = pkt_index
    #         last_sent = pkt_index
    #
    #     while packets_received < num_of_packets:
    #         ack = int(self.udp_socket.recvfrom(MSG_SIZE)[0].decode())
    #         index_in_expected = expected.index(ack)
    #         if index_in_expected == 0:
    #             self.udp_socket.sendto(packets[last_sent + 1], addr)
    #             expected = expected[:-1] + [last_sent + 1]
    #             packets_received += 1
    #             last_sent += 1
    #         else:
    #             for pkt_index in expected[:index_in_expected]:
    #                 self.udp_socket.sendto(packets[pkt_index], addr)
    #             self.udp_socket.sendto(packets[expected_ack], addr)

    #
    # def send_packets(self, packets: list, addr):
    #     if len(packets) < 4:
    #         self.window_size = [0, 1, 2, 3]
    #     else:
    #         self.window_size = list(range(0, len(packets)))
    #     print("sending number of packets..")
    #     self.udp_socket.sendto(str(len(packets)).encode(), addr)
    #     ack_handler_thread = threading.Thread(target=self.ack_handler, args=(packets,))
    #     ack_handler_thread.start()
    #     while len(packets) > 0:
    #         time.sleep(0.1)
    #         for pkt in self.window_size:
    #             print(f"sending packet number: {pkt}")
    #             self.udp_socket.sendto(packets[pkt], addr)
    #             self.window_size.remove(pkt)
    #
    # def ack_handler(self, packets: list):
    #     expected_ack = 0
    #     # arrived = 0
    #     next_packet = len(self.window_size)
    #     time.sleep(0.2)
    #     while len(packets) > 0:
    #         time.sleep(0.1)
    #         ack = int(self.udp_socket.recvfrom(MSG_SIZE)[0].decode())
    #         if ack == expected_ack:
    #             print(f"adding next packet {next_packet} to window")
    #             self.window_size.append(next_packet)
    #             packets.remove(ack)
    #             print(len(packets))
    #             # arrived += 1
    #         else:
    #             print(f"adding AGAIN packet {expected_ack} to window")
    #             self.window_size.append(expected_ack)

    # def send_packets(self, packets: list, addr):
    #     window = [0, 1, 2, 3]
    #     packets_not_sent: list = list(range(0, len(packets)))
    #     print("sending number of packets..")
    #     self.udp_socket.sendto(str(len(packets)).encode(), addr)
    #     print("sent successfully")
    #     print("sending window..")
    #     self.send_window(window, packets, addr)
    #     # thread_acks = threading.Thread(target=self.handle_acks, args=(packets, addr, packets_not_sent))
    #     # thread_acks.start()
    #     self.handle_acks(packets, addr, packets_not_sent)
    #
    # def handle_acks(self, packets, addr, packets_not_sent: list):
    #     expected_ack = 0
    #     next_packet = 4
    #     while expected_ack < len(packets):
    #         time.sleep(0.1)
    #         ack = int(self.udp_socket.recvfrom(MSG_SIZE)[0].decode())
    #         print(f"server received packet: {ack}")
    #         if ack == expected_ack:
    #             if ack + 4 < len(packets):
    #                 self.send_packet(packets, next_packet, addr)
    #                 next_packet += 1
    #                 # packets_not_sent.remove(ack)
    #                 print(f"expected_ack: {expected_ack}")
    #             expected_ack += 1
    #         else:
    #             print(f"wrong ack, sending again {expected_ack} -{ack-1}")
    #             # self.send_packet(packets, expected_ack, addr)
    #             new_window = list(range(expected_ack, ack - 1))
    #             self.send_window(new_window, packets, addr)
    #
    # def send_window(self, window_size, packets: list, addr):
    #     for pkt in window_size:
    #         self.udp_socket.sendto(packets[pkt], addr)
    #
    # def send_packet(self, packets: list, pkt, addr):
    #     if pkt < len(packets):
    #         self.udp_socket.sendto(packets[pkt], addr)
    #
    # def check_lost_pkts(self, aks_sent: dict, exepcted_akcs: dict, packets_not_sent: list):
    #     arrived = 0
    #     for key in exepcted_akcs.keys():
    #         if aks_sent[key] == exepcted_akcs[key]:
    #             print(f"packet number {key} sent successfully, removing from list..")
    #             packets_not_sent.remove(key)
    #             arrived += 1
    #     return arrived

    def check_reliablity(self, data, addr):
        if data.decode() == "ACK":
            self.udp_socket.sendto("SYN".encode(), addr)
            data, _ = self.udp_socket.recvfrom(MSG_SIZE)
            return True
        return False

# todo thread to disconnect and close all ports from console
# for pkt in range(window_size):
# expected_acks.clear()
#     if pkt in packets_not_sent:
#         print(f"sending packet number: {pkt}")
#         self.udp_socket.sendto(packets[pkt], addr)
#         ack = self.udp_socket.recvfrom(MSG_SIZE)[0].decode()
#         print("received response from client")
#         ack = ack.split(',')
#         print(f"{ack[1]}, {ack[0]}")
#         acks_sent[int(ack[1])] = ack[0]
#         expected_acks[pkt] = "ACK"
#         print("-------------")
# arrived = self.check_lost_pkts(acks_sent, expected_acks, packets_not_sent)
# print(f"arrived {arrived} packets")
# window_size += arrived
