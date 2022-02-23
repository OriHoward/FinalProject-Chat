import threading
from tkinter import DISABLED

from SocketHandler import SocketHandler

PREFIX = '/'


class HandleThreads:
    def __init__(self, display_chat, button_action):
        self.display_chat = display_chat
        self.button_action = button_action

    def start_receiver(self):
        thread = threading.Thread(target=self.receive, daemon=True)
        thread.start()

    def start_sender(self, msg):
        thread = threading.Thread(target=self.sender, args=(msg,), daemon=True)
        thread.start()

    def receive(self):
        while self.display_chat.user.is_connected:
            try:
                message = SocketHandler.get_msg(self.display_chat.user.server)
                self.display_chat.update_chat_display(message)
            except:
                print("An error occurred in receive thread")
                self.button_action.handle_disconnect()
        self.display_chat.window.destroy()

    def sender(self, msg):
        self.display_chat.text_cons.config(state=DISABLED)
        if len(msg) > 0:
            if msg[0] == PREFIX:
                self.display_chat.user.action_received(msg)
            else:
                self.display_chat.user.send_msg_to_all()
                message = f"{self.display_chat.name}: {msg}"
                SocketHandler.send_msg(message, self.display_chat.user.server)
