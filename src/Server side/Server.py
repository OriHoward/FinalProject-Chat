import socket
import threading
from HandleClients import HandleClients
from Client import Client
from Actions import Actions
from socketHandler import socketHandler

FORMAT = 'utf-8'
HEADER = 64
PORT = 55000
IP = socket.gethostbyname(socket.gethostname())
ADDR = (IP, PORT)


class Server:
    def __init__(self):
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.bind(ADDR)
        self.handler = HandleClients()

    def handle_client(self, conn, addr, curr_client: Client):
        print(f"[NEW CONNECTION] {addr} connected.")
        while curr_client.is_connected:
            action_msg = conn.recv(1)
            if action_msg:
                self.handle_msg(action_msg, curr_client)
        conn.close()

    def start(self):
        print("[LISTENING] server is now listening..")
        self.server.listen()
        while True:
            conn, addr = self.server.accept()
            curr_name = socketHandler.get_msg(conn)
            new_client = Client(conn, curr_name)
            self.handler.add_client(new_client)
            thread = threading.Thread(target=self.handle_client, args=(conn, addr, new_client))
            thread.start()
            print(f"[ACTIVE CONNECTIONS] {threading.active_count() - 1}")

    def handle_msg(self, msg, curr_client):
        match msg:
            case Actions.USER_LIST.value:
                self.send_user_list(curr_client.client_socket)
            case Actions.PRIVATE_MSG.value:
                self.handle_private_msg(curr_client)
            case Actions.DISCONNECT.value:
                curr_client.disconnect()
            case Actions.MESSAGE_ALL.value:
                msg_to_distribute = socketHandler.get_msg(curr_client.client_socket)
                self.send_msg_all(msg_to_distribute)

    def send_user_list(self, conn):
        client_list = ""
        for client in self.handler.get_clients():
            client_list += f"{client},"
        client_list = client_list[:len(client_list) - 1]
        socketHandler.send_msg(client_list, conn)

    def handle_private_msg(self, client: Client):
        dst_client_user_name = socketHandler.get_msg(client.client_socket)
        dst_client_socket = self.handler.get_client(dst_client_user_name).client_socket
        msg = socketHandler.get_msg(client.client_socket)
        socketHandler.send_msg(msg, dst_client_socket)

    def send_msg_all(self, msg):
        self.handler.send_all(msg)
