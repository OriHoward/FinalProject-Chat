import os
import socket
from struct import unpack

from Client import Client
from SocketHandler import SocketHandler

GATEWAY_PORT = 50000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, GATEWAY_PORT)


class HandleClients:
    def __init__(self):
        self.clients: dict[str, Client] = {}

    def add_client(self, client: Client):
        self.clients[client.user_name] = client

    def get_clients(self):
        return self.clients.keys()

    def remove_client(self, client: Client):
        self.clients.pop(client.user_name, None)

    def get_client(self, key):
        return self.clients.get(key)

    def send_all(self, msg):
        for client in self.clients.values():
            SocketHandler.send_msg(msg, client.client_socket)

    def send_user_list(self, conn):
        client_list = ""
        for client in self.get_clients():
            client_list += f"{client},"
        client_list = client_list[:len(client_list) - 1]
        SocketHandler.send_msg(f"Connected users: {client_list}", conn)

    def handle_private_msg(self, client: Client):
        dst_client_user_name = SocketHandler.get_msg(client.client_socket)
        if self.clients.get(dst_client_user_name) is None:
            SocketHandler.send_msg(f"Client {dst_client_user_name} is not connected", client.client_socket)
        else:
            dst_client_socket = self.get_client(dst_client_user_name).client_socket
            msg = SocketHandler.get_msg(client.client_socket)
            msg_to = f"to {dst_client_user_name}: {msg}"
            SocketHandler.send_msg(msg_to, client.client_socket)
            msg_from = f"from {client.user_name}: {msg}"
            SocketHandler.send_msg(msg_from, dst_client_socket)

    def disconnect(self, client):
        self.remove_client(client)
        client.disconnect()
        self.send_all(f"{client.user_name} has disconnected")

    def is_user_available(self, name):
        if self.get_client(name) is None:
            return True
        return False

    def send_file_list(self, client: Client):
        files_path = os.path.abspath("ServerFiles")
        files_list = os.listdir(files_path)
        msg = f"Server file list: {str(files_list)}"
        SocketHandler.send_msg(msg, client.client_socket)

    def send_commands_list(self, client: Client):
        msg = "/users , /disconnect , /files , /whisper <client name> <msg>"
        SocketHandler.send_msg(msg, client.client_socket)

    # def send_file(self, server_udp: socket, file):
    #     offset = 0
    #     sequence_num = 0
    #     segment = 2
    #     packets_list = self.create_packet_list(file, segment)
    #
    # # def create_packet(self, packet_list, sequence):
    # #     pass
    #
    # def create_packet_list(self, file, segment):
    #
    #     packets = []


    def checksum(self):
        def calculate_icmpv6_checksum(packet):
            total = 0
            num_words = len(packet) // 2
            for chunk in unpack("!%sH" % num_words, packet[0:num_words * 2]):
                total += chunk

            if len(packet) % 2:
                total += packet[-1] << 8

            total = (total >> 16) + (total & 0xffff)
            total += total >> 16
            return ~total + 0x10000 & 0xffff
