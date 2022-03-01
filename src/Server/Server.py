import functools
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
        self.udp_ports = [False for _ in range(10)]
        self.window: list = []
        self.handler = HandleClients()

    """
        handle the client in a loop in each message taking care of
    """

    def handle_client(self, conn, addr, curr_client: Client):
        print(f"[NEW CONNECTION] {addr} connected.")
        while curr_client.is_connected:
            action_msg = SocketHandler.get_enum(conn)
            if action_msg:
                self.handle_msg(action_msg, curr_client)
        conn.close()

    """
        starts listening.. opens thread for each client that the server accepts
    """

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
            udp_port = self.get_udp_port()
            self.handler.send_message(str(udp_port), conn)
            self.handler.send_all(f"SERVER: {curr_name} has joined the chat!")
            self.handler.send_message("SERVER: connection successful!", conn)
            self.handler.send_message("SERVER: type /commands for more options", conn)
            new_client = Client(conn, curr_name, addr, udp_port)
            self.handler.add_client(new_client)
            tcp_thread = threading.Thread(target=self.handle_client, args=(conn, addr, new_client), daemon=True)
            tcp_thread.start()

            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")

    """
        handles each message from the client with case match
    """

    def handle_msg(self, msg, client):
        if msg == Actions.USER_LIST.value:
            self.handler.send_user_list(client.client_socket)
        elif msg == Actions.PRIVATE_MSG.value:
            self.handler.handle_private_msg(client)
        elif msg == Actions.DISCONNECT.value:
            self.udp_ports[client.udp_port - 60000] = False
            self.handler.disconnect(client)
        elif msg == Actions.MESSAGE_ALL.value:
            msg_to_distribute = SocketHandler.get_msg(client.client_socket)
            self.handler.send_all(msg_to_distribute)
        elif msg == Actions.FILE_LIST.value:
            self.handler.send_file_list(client)
        elif msg == Actions.COMMANDS.value:
            self.handler.send_commands_list(client)
        elif msg == Actions.OPEN_UDP.value:
            thread = threading.Thread(target=self.handle_client_udp, args=(client,))
            thread.start()

    """
        handle the UDP connection from the client for the file transfer
    """

    def handle_client_udp(self, client: Client):
        start_time = time.time()
        udp_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        udp_socket.bind((IP, client.udp_port))
        try:
            udp_socket.settimeout(0.1)
            data, addr = udp_socket.recvfrom(MSG_SIZE)
        except:
            udp_socket.close()
            return
        if not self.check_reliablity(data, addr, udp_socket):
            udp_socket.close()
            return
        file_name = udp_socket.recvfrom(MSG_SIZE)[0].decode()
        if not self.handler.check_file_name(file_name):
            self.handler.send_message("SERVER: file name not found, try again.", client.client_socket)
            udp_socket.sendto("w".encode(), addr)
            udp_socket.close()
            return
        else:
            udp_socket.sendto("g".encode(), addr)
            file_path = f'ServerFiles/{file_name}'
            packets = self.create_packets_list(file_path)
            cut_list_index = len(packets) / 2
            first_half_packets = packets[:int(cut_list_index)]
            second_half_packets = packets[int(cut_list_index):]
            if not client.received_half_file:
                client.set_received_half_file(True)
                self.handler.send_message("---Opening UDP connection, RDT in action---", client.client_socket)
                self.handler.send_message("---Downloading file---", client.client_socket)
                self.send_packets_selective_repeat(first_half_packets, addr, int(cut_list_index), False, udp_socket)
                last_byte = self.get_last_file_bytes(file_path, False)
                self.handler.send_message("---Downloaded 50% of the file successfully---", client.client_socket)
                self.handler.send_message("---Click proceed to download second half---", client.client_socket)
                self.handler.send_message(f"---Last byte: {last_byte}---", client.client_socket)
                total_time = format(time.time() - start_time, ".2f")
                self.handler.send_message(f"---Finished downloading first half of file. TOTAL TIME: {total_time}---",
                                          client.client_socket)
                udp_socket.close()
            else:
                client.set_received_half_file(False)
                self.handler.send_message("---Reopening UDP connection, RDT in action---", client.client_socket)
                self.handler.send_message("---Proceeding to download file---", client.client_socket)
                self.send_packets_selective_repeat(second_half_packets, addr, int(cut_list_index), True, udp_socket)
                last_byte = self.get_last_file_bytes(file_path, True)
                self.handler.send_message("--FINISHED---", client.client_socket)
                self.handler.send_message(f"---Last byte: {last_byte}---", client.client_socket)
                total_time = format(time.time() - start_time, ".2f")
                self.handler.send_message(f"---Finished downloading file. TOTAL TIME: {total_time}---",
                                          client.client_socket)
                udp_socket.close()

    """
        return the last byte of a file
    """

    def get_last_file_bytes(self, file_path, is_second_part):
        f = open(file_path, 'rb')
        file_size = os.path.getsize(file_path)
        if is_second_part:
            return int.from_bytes(f.read()[-1:], byteorder='little', signed=True)
        else:
            return int.from_bytes(f.read()[int(file_size / 2):int(file_size / 2) + 1], byteorder='little', signed=True)

    """
        reading small data fragments of the file and creates packets from each data
        chunk then adding all packets to list and returns it
    """

    def create_packets_list(self, file_path):
        packets = []
        sequence_num = 0
        file_size = os.path.getsize(file_path)
        slice_to_read = 10000
        if file_size < slice_to_read:
            slice_to_read = int(file_size / 6)
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

    """
        this function sending the packets to client with a method called:
        'selective repeat'. we have a sliding window size which sends the packets.
        for each ack the server receives we send another packet, the size of the window
        is always the number of packets that are in flight. if a packet got lost or timed out,
        we send the packet again.
    """

    def send_packets_selective_repeat(self, packets: list, addr, num, is_second_part, conn):
        num_of_packets = len(packets)
        packets_to_append = 2
        done = False
        self.window = [0, 1, 2, 3]
        next_packet = 4
        skip = False
        cut_window = 0
        conn.sendto(str(num_of_packets).encode(), addr)
        self.send_window(packets, addr, conn)
        while not done:
            if is_second_part:
                expected_ack = self.window[0] + num
            else:
                expected_ack = self.window[0]
            ack = int(conn.recvfrom(MSG_SIZE)[0].decode())
            if ack == -10:
                done = True
            elif ack == -1:
                packets_to_append = 1
                # TODO cut window in 2 if timeout and resend
                print("resending  ALL packets and RESET window size")
                self.send_window(packets, addr, conn)
            elif ack == expected_ack:
                if skip:
                    print(f"got ack : {ack} now removing from list")
                    self.window.remove(self.window[0])
                    if cut_window >= int(len(self.window)):
                        skip = False
                    continue
                print(f"packets to append is up to: {packets_to_append}")
                print(f"ack received successfully: {ack}")
                if next_packet < len(packets):
                    conn.sendto(packets[next_packet], addr)
                    self.window.append(next_packet)
                    self.window.remove(self.window[0])
                    next_packet += 1
                    print("sliding window..")
                    print(f"appending next packet : {self.window}")
                for i in range(packets_to_append):
                    if next_packet < len(packets):
                        conn.sendto(packets[next_packet], addr)
                        self.window.append(next_packet)
                        next_packet += 1

            else:
                if not skip:
                    skip = True
                    cut_window = int(len(self.window) / 2)
                print(f"packets to append is down to: {packets_to_append}")
                if is_second_part:
                    ack_to_cut = self.window.index(ack - num)
                else:
                    ack_to_cut = self.window.index(ack)
                print(f"ack received: {ack}")
                resend = self.window[:ack_to_cut]
                print(f"resending: {resend}")
                self.resend_packets(packets, resend, addr, conn)
                if ack_to_cut < len(self.window) - 1:
                    first_part = self.window[(ack_to_cut + 1):]
                    second_part = self.window[:ack_to_cut]
                    self.window = first_part + second_part
                else:
                    self.window = self.window[:ack_to_cut]

    """
        appending new packets according to the window size
    """

    # def append_packets(self, packets_to_append, packets: list, addr, conn, next_packet):
    #     for i in range(packets_to_append):
    #         if next_packet < len(packets):
    #             conn.sendto(packets[next_packet], addr)
    #             self.window.append(next_packet)
    #             print(f"this is the current size: {self.window}")
    #             next_packet += 1
    #             print(f"packet number in function: {next_packet}")
    #             # print("adding packet to increase window..")
    #             # print(f"window now is: {self.window}")

    """
        sends the whole window.
    """

    def send_window(self, packets: list, addr, conn):
        for pkt in self.window:
            conn.sendto(packets[pkt], addr)

    """
        resending the packets who got lost
    """

    def resend_packets(self, packets: list, resend: list, addr, conn):
        for pkt in resend:
            if pkt < len(packets):
                conn.sendto(packets[pkt], addr)

    """
        checking reliability with the client with the 'three way handshake' method 
        before transferring the file
    """

    def check_reliablity(self, data, addr, conn):
        try:
            if data.decode() == "ACK":
                conn.sendto("SYN".encode(), addr)
                conn.settimeout(0.1)
                data, _ = conn.recvfrom(MSG_SIZE)
                return True
        except:
            return False

    def get_udp_port(self):
        for port, unavailable in enumerate(self.udp_ports):
            if not unavailable:
                self.udp_ports[port] = True
                return 60000 + port
        return -1

    def __del__(self):
        print("DESTOYRED")
        self.server.close()
