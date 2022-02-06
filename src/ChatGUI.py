import threading
from tkinter import *

from HandleClients import HandleClients
from User import User
from socketHandler import socketHandler

PREFIX = '/'
handler = HandleClients()


class ChatGUI:
    def __init__(self):
        self.window = Tk()
        self.window.withdraw()
        self.login = Toplevel()
        self.login.title("Login")
        self.login.resizable(width=False, height=False)
        self.login.configure(width=450, height=200)
        self.login_label = Label(self.login, text="Please login to continue", justify=CENTER, font="Helvetica 14 bold")
        self.login_label.place(relheight=0.1, relx=0.2, rely=0.07)
        self.clients = handler.clients.keys()
        self.label_name = Label(self.login, text="Enter your name: ", font="Helvetica 12")
        self.label_name.place(relheight=0.1, relx=0.03, rely=0.2)

        self.entry_name = Entry(self.login, font="Helvetica 14")
        self.entry_name.place(relwidth=0.4, relheight=0.12, relx=0.35, rely=0.2)

        self.entry_name.focus()

        self.btn = Button(self.login, text="CONTINUE", font="Helvetica 14 bold",
                          command=lambda: self.enter_main_window(self.entry_name.get()))
        self.btn.place(relx=0.4, rely=0.4)

        self.name = None
        self.text_cons = None
        self.new_user: User = None
        self.msg = None

        self.window.mainloop()

    def enter_main_window(self, name):
        self.new_user = User(name)
        self.new_user.connect()
        self.login.destroy()
        self.layout(name)
        rcv = threading.Thread(target=self.receive)
        rcv.start()

    # exmaple to use later: for clicked.get() and .pack()
    # new_button = Button(self.window, text="do something", command=self.do_something_accordingly())

    def layout(self, name):
        self.name = name
        self.window.deiconify()
        self.window.title("Welcome to the chat room")
        self.window.resizable(width=False, height=False)
        self.window.configure(width=470, height=550, bg="#17202A")

        name_label = Label(self.window, bg="#17202A", fg="#EAECEE", text=self.name, font="Helvetica 13 bold", pady=5)
        name_label.place(relwidth=1)

        line = Label(self.window, width=450, bg="#ABB2B9")
        line.place(relwidth=1, rely=0.07, relheight=0.012)

        self.text_cons = Text(self.window, width=20, height=2, bg="#17202A", fg="#EAECEE", font="Helvetica 14", padx=5,
                              pady=5)
        self.text_cons.place(relheight=0.745, relwidth=1, rely=0.08)

        bottom_label = Label(self.window, bg="#ABB2B9", height=80)
        bottom_label.place(relwidth=1, rely=0.825)

        entry_msg = Entry(bottom_label, bg="#2C3E50", fg="#EAECEE", font="Helvetica 13")
        entry_msg.place(relwidth=0.74, relheight=0.06, rely=0.008, relx=0.011)

        entry_msg.focus()

        # send_btn_msg = Button(bottom_label, text="Send", font="Helvetica 10 bold", width=20, bg="#ABB2B9",
        #                       command=lambda: self.send_button(entry_msg.get(), entry_msg))
        # send_btn_msg.place(relx=0.77, rely=0.008, relheight=0.03, relwidth=0.22)

        # sending the message with ENTER key:
        self.window.bind('<Return>', lambda event: self.send_msg(event, entry_msg.get(), entry_msg))

        clicked = StringVar()
        clicked.set("Send to")
        drop = OptionMenu(self.window, clicked, self.clients)
        drop.configure(bg="#ABB2B9", fg="#EAECEE", font="Helvetica 10 bold", width=20)
        drop.pack()

        drop.place(relx=0.77, rely=0.9, relheight=0.05, relwidth=0.22)
        # todo add to send button option with clicked.get() to know which one was chosen

        self.text_cons.config(cursor="arrow")

        scrollbar = Scrollbar()
        scrollbar.place(relheight=1, relx=0.974)
        scrollbar.config(command=self.text_cons.yview)
        self.text_cons.config(state=DISABLED)

    def send_msg(self, event, msg, entry_msg):
        self.text_cons.config(state=DISABLED)
        self.msg = msg
        entry_msg.delete(0, END)
        snd = threading.Thread(target=self.send_message)
        snd.start()

    # def send_button(self, msg, entry_msg):
    #     self.text_cons.config(state=DISABLED)
    #     self.msg = msg
    #     entry_msg.delete(0, END)
    #     snd = threading.Thread(target=self.send_message)
    #     snd.start()

    def receive(self):
        while True:
            try:
                message = socketHandler.get_msg(self.new_user.server)
                self.text_cons.config(state=NORMAL)
                self.text_cons.insert(END, message + "\n")
                self.text_cons.config(state=DISABLED)
                self.text_cons.see(END)
            except:
                print("An error occured!")
                self.new_user.disconnect()
                break

    def send_message(self):
        self.text_cons.config(state=DISABLED)
        while True:
            if self.msg[0] == PREFIX:
                self.new_user.action_received(self.msg)
                break
            # if self.msg[0] == PREFIX:
            #     match self.msg[1:]:
            #         case "get_users":
            #             self.new_user.get_users()
            #         case "disconnect":
            #             self.new_user.disconnect()
            #         case "get_file_list":
            #             pass
            else:
                self.new_user.send_msg_to_all()
                message = (f"{self.name}: {self.msg}")
                socketHandler.send_msg(message, self.new_user.server)
                break
