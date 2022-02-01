class Client:
    def __init__(self, client_socket, user_name):
        self.client_socket = client_socket
        self.user_name = user_name
        self.is_connected = True

    def __repr__(self):
        return f"user_name:{self.user_name}"

    def disconnect(self):
        print(f"{self.user_name} has disconnected")
        self.is_connected = False