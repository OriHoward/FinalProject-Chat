from Server import Server


class Client:
    def __init__(self, client_socket, user_name):
        self.client_socket = client_socket
        self.user_name = user_name

    def __repr__(self):
        return f"user_name:{self.user_name}"
