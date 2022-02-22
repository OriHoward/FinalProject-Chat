class ButtonsActions:
    def __init__(self, display_chat):
        self.display_chat = display_chat

    def handle_disconnect(self):
        self.display_chat.new_user.disconnect()
        self.display_chat.window.destroy()

    def handle_file_download(self, file_name):
        if len(file_name) > 0:
            self.display_chat.new_user.download_file(file_name)
