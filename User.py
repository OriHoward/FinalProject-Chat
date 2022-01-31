import socket



class User:
    def __init__(self, name: str = "", _id: int = None):
        self.name = name
        self._id = _id
        self.client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    #
    # def send_message_to(self, msg, client):
    #     pass
    #
    # def get_list_file(self):
    #     pass
    #
    # def request_file(self):
    #     pass
    #
    # def download_file(self):
    #     pass
