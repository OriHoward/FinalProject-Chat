class ButtonsActions:
    def __init__(self, display_chat):
        self.display_chat = display_chat
        self.file_name = None

    def handle_disconnect(self):
        self.display_chat.user.disconnect()
        self.display_chat.window.destroy()

    def handle_file_download(self, file_name, file_window):
        self.file_name = file_name
        if len(file_name) > 0:
            if self.display_chat.user.download_file(file_name):
                file_window.destroy()
                self.display_chat.pop_proceed_window()

    def proceed_download(self, proceed_window):
        self.display_chat.user.download_file(self.file_name)
        proceed_window.destroy()

    def exit_window(self, window):
        window.destroy()

    def handle_udp_connection(self):
        self.display_chat.user.open_udp_connection()
