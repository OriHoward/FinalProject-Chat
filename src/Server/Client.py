class Client:
    def __init__(self, client_socket, user_name, addr):
        self.client_socket = client_socket
        self.user_name = user_name
        self.address = addr
        self.is_connected = True
        self.new_msgs = []

    def __repr__(self):
        return f"user_name:{self.user_name}"

    def disconnect(self):
        self.is_connected = False
        self.client_socket.close()

    def get_new_msg(self):
        if self.new_msgs:
            return self.new_msgs.pop(0)
