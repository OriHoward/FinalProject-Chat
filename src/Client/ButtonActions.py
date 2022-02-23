class ButtonsActions:
    def __init__(self, display_chat):
        self.display_chat = display_chat

    def handle_disconnect(self):
        self.display_chat.user.disconnect()
        self.display_chat.window.destroy()

    def handle_file_download(self, file_name):
        print(len(file_name))
        if len(file_name) > 0:
            self.display_chat.user.download_file(file_name)
    def handle_udp_connection(self):
        self.display_chat.user.open_udp_connection()
