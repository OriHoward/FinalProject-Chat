import socket
import threading

from Actions import Actions
from Client import Client
from HandleClients import HandleClients
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT_TCP = 50000
GATEWAY_PORT_UDP = 60000

IP = socket.gethostbyname(socket.gethostname())
ADDR_TCP = (IP, GATEWAY_PORT_TCP)
ADDR_UDP = (IP, GATEWAY_PORT_UDP)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR_TCP)
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
                continue
            self.handler.send_all(f"{curr_name} has joined the chat!")
            SocketHandler.send_msg("connection successful!", conn)
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
        server_udp = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        server_udp.bind(ADDR_UDP)
        if self.check_reliablity(server_udp):
            SocketHandler.send_msg("opened UDP connection, ready to download", client.client_socket)
        # self.handler.send_file(server_udp, "babibu")
        server_udp.close()

    def check_reliablity(self, server_udp):
        data, addr = server_udp.recvfrom(3)
        if data.decode() == "ACK":
            server_udp.sendto("SYN".encode(), addr)
            data, _ = server_udp.recvfrom(3)
            return True
        return False

    # def fake_server(self):
    #     self.server.listen(0)
    #     self.server.accept()
    #     self.server.close()

    # todo thread to disconnect and close all ports from console
