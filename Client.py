import socket


class Client:
    def __init__(self, name, _id):
        self.name = name
        self._id = _id

    def connect(self):
        pass

    def disconnect(self):
        pass

    def send_message_to(self, client):
        pass

    def send_message_to_all(self):
        pass

    def get_users(self):
        pass

    def get_list_file(self):
        pass

    def request_file(self):
        pass

    def download_file(self):
        pass


