import socket
import threading
from HandleClients import HandleClients
from Client import Client
from Actions import Actions
from SocketHandler import SocketHandler

FORMAT = 'utf-8'
GATEWAY_PORT = 50000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, GATEWAY_PORT)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR)
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
            new_client = Client(conn, curr_name)
            self.handler.add_client(new_client)
            thread = threading.Thread(target=self.handle_client, args=(conn, addr, new_client), daemon=True)
            thread.start()
            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")

    def handle_msg(self, msg, curr_client):
        match msg:
            case Actions.USER_LIST.value:
                self.handler.send_user_list(curr_client.client_socket)
            case Actions.PRIVATE_MSG.value:
                self.handler.handle_private_msg(curr_client)
            case Actions.DISCONNECT.value:
                self.handler.disconnect(curr_client)
            case Actions.MESSAGE_ALL.value:
                msg_to_distribute = SocketHandler.get_msg(curr_client.client_socket)
                self.handler.send_all(msg_to_distribute)

    # todo thread to disconnect and close all ports from console
