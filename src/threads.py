import threading
from tkinter import DISABLED

from socketHandler import socketHandler

PREFIX = '/'


class handle_Threads:
    def __init__(self, display_chat):
        self.display_chat = display_chat

    def start_receiver(self):
        thread = threading.Thread(target=self.receive)
        thread.start()

    def start_sender(self, msg):
        thread = threading.Thread(target=self.sender, args=(msg,))
        thread.start()

    def receive(self):
        while self.display_chat.new_user.is_connected:
            try:
                message = socketHandler.get_msg(self.display_chat.new_user.server)
                self.display_chat.update_chat_display(message)
            except:
                print("An error occured!")
                self.display_chat.handle_disconnect()

    def sender(self, msg):
        self.display_chat.text_cons.config(state=DISABLED)
        if len(msg) > 0:
            if msg[0] == PREFIX:
                self.display_chat.new_user.action_received(msg)
            else:
                self.display_chat.new_user.send_msg_to_all()
                message = f"{self.display_chat.name}: {msg}"
                socketHandler.send_msg(message, self.display_chat.new_user.server)
