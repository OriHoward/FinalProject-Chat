from tkinter import *
from tkinter import messagebox
import ButtonActions
import threads
from Actions import Actions
from SocketHandler import SocketHandler
from Client import Client


class ChatGUI:
    def __init__(self, user: Client):
        self.window = Tk()
        self.window.withdraw()
        self.login = Toplevel()
        self.login.title("Login")
        self.login.resizable(width=False, height=False)
        self.login.configure(width=650, height=400)
        self.login_label = Label(self.login, text="Please login to continue", justify=CENTER, font="Helvetica 14 bold")
        self.login_label.place(relheight=0.1, relx=0.35, rely=0.07)
        self.label_name = Label(self.login, text="Enter your name: ", font="Helvetica 12")
        self.label_name.place(relheight=0.1, relx=0.03, rely=0.2)
        self.label_address = Label(self.login, text="Enter server ip: ", font="Helvetica 12")
        self.label_address.place(relheight=0.1, relx=0.03, rely=0.4)
        self.window.protocol("WM_DELETE_WINDOW", self.close_chat)
        self.login.protocol("WM_DELETE_WINDOW", self.close_login)
        self.entry_name = Entry(self.login, font="Helvetica 14")
        self.entry_name.place(relwidth=0.4, relheight=0.12, relx=0.35, rely=0.2)
        self.entry_address = Entry(self.login, font="Helvetica 14")
        self.entry_address.place(relwidth=0.4, relheight=0.12, relx=0.35, rely=0.4)
        self.entry_name.focus()
        self.button_action = ButtonActions.ButtonsActions(self)
        self.thread = threads.HandleThreads(self, self.button_action)
        self.user = user
        self.btn = Button(self.login, text="CONTINUE", font="Helvetica 14 bold",
                          command=lambda: self.validation(self.entry_name.get(), self.entry_address.get()))
        self.btn.place(relx=0.45, rely=0.58)
        self.download_button = None
        self.name = None
        self.text_cons = None
        self.msg = None

        self.window.mainloop()

    def validation(self, name, address):
        if not self.is_valid(name):
            self.error_msg()
            return
        self.user.set_address(address)
        self.user.set_username(name)
        self.user.connect()
        if not self.is_connected():
            self.connect_error_msg()
            return
        if not self.is_name_free():
            self.user.connected = False
            self.taken_name_msg()
            self.user.close_socket()
            return
        self.user.udp_address = (self.user.ip, int(SocketHandler.get_msg(self.user.tcp_socket)))
        self.enter_main_window(name)

    def enter_main_window(self, name):
        self.login.destroy()
        self.layout(name)
        self.thread.start_receiver()

    def layout(self, name):
        self.name = name
        self.window.deiconify()
        self.window.title("Welcome to the chat room")
        self.window.resizable(width=True, height=True)
        self.window.configure(width=800, height=550, bg="#17202A")
        self.add_chat_window()
        self.add_buttons()
        self.text_cons.config(cursor="arrow")
        self.add_scrollbar()

    def update_chat_display(self, message):
        if self.user.connected:
            self.text_cons.config(state=NORMAL)
            self.text_cons.insert(END, message + "\n")
            self.text_cons.config(state=DISABLED)
            self.text_cons.see(END)

    def add_buttons(self):
        disconnect_btn = Button(self.window, text="Disconnect", font="Helvetica 10 bold", width=20, bg="#ABB2B9",
                                command=lambda: self.button_action.handle_disconnect())
        disconnect_btn.place(relx=0.78, rely=0.85, relheight=0.04, relwidth=0.15)
        self.download_button = Button(self.window, text="Download File", font="Helvetica 10 bold", width=20,
                                      bg="#ABB2B9"
                                      , command=lambda: self.file_download_window())
        self.download_button.place(relx=0.78, rely=0.89, relheight=0.04, relwidth=0.15)
        show_commands_btn = Button(self.window, text="Show Commands", font="Helvetica 10 bold", width=20, bg="#ABB2B9",
                                   command=lambda: self.button_action.handle_show_commands())
        show_commands_btn.place(relx=0.78, rely=0.95, relheight=0.04, relwidth=0.15)

    def add_chat_window(self):
        name_label = Label(self.window, bg="#17202A", fg="#EAECEE", text=self.name, font="Helvetica 13 bold", pady=5)
        name_label.place(relwidth=1)
        line = Label(self.window, width=450, bg="#ABB2B9")
        line.place(relwidth=1, rely=0.07, relheight=0.012)
        bottom_label = Label(self.window, bg="#ABB2B9", height=80)
        bottom_label.place(relwidth=1, rely=0.825)
        self.text_cons = Text(self.window, width=20, height=2, bg="#17202A", fg="#EAECEE", font="Helvetica 14", padx=5,
                              pady=5)
        self.text_cons.place(relheight=0.745, relwidth=1, rely=0.08)
        entry_msg = Entry(bottom_label, bg="#2C3E50", fg="#EAECEE", font="Helvetica 13")
        entry_msg.place(relwidth=0.74, relheight=0.06, rely=0.008, relx=0.011)
        entry_msg.focus()

        self.window.bind('<Return>', lambda event: self.send_msg(event, entry_msg.get(), entry_msg))

    def send_msg(self, event, msg, entry_msg):
        self.text_cons.config(state=DISABLED)
        entry_msg.delete(0, END)
        self.thread.start_sender(msg)

    def add_scrollbar(self):
        scrollbar = Scrollbar()
        scrollbar.place(relheight=1, relx=0.98)
        scrollbar.config(command=self.text_cons.yview)
        self.text_cons.config(state=DISABLED)

    def close_chat(self):
        if messagebox.askokcancel("Quit", "Are you sure you want to disconnect?"):
            self.button_action.handle_disconnect()

    def close_login(self):
        if messagebox.askokcancel("Quit", "Are you sure you want to exit?"):
            quit()

    def error_msg(self):
        error = Label(self.login, text="---please enter a valid one word name---", font="Helvetica 12")
        error.place(relheight=0.1, relx=0.33, rely=0.7)

    def is_available(self):
        if SocketHandler.get_enum(self.user.tcp_socket) == Actions.TRUE.value:
            return True
        return False

    def taken_name_msg(self):
        error = Label(self.login, text="---Name is taken, please try a different name---", font="Helvetica 12")
        error.place(relheight=0.1, relx=0.33, rely=0.7)

    def file_download_window(self):
        file_window = Toplevel(self.window)
        file_label = Label(file_window, text="Enter file name to download",
                           font="Helvetica 14 bold")
        file_label.place(relheight=0.1, relx=0.25, rely=0.07)
        entry_file_name = Entry(file_window, font="Helvetica 14")
        entry_file_name.place(relwidth=0.5, relheight=0.15, relx=0.27, rely=0.3)
        button_to_create = Button(self.window, text="Proceed", font="Helvetica 14 bold", bg="#ABB2B9",
                                  command=lambda: self.button_action.proceed_download())
        download_btn = Button(file_window, text="Download", font="Helvetica 14 bold",
                              command=lambda: self.button_action.handle_file_download(entry_file_name.get(),
                                                                                      file_window,
                                                                                      self.download_button,
                                                                                      button_to_create))
        download_btn.place(relx=0.40, rely=0.58)
        file_window.title("Choose your file")
        file_window.configure(width=500, height=150)

    def swap_buttons(self, button_to_destroy, button_to_create):
        button_to_destroy.destroy()
        button_to_create.place(relx=0.78, rely=0.89, relheight=0.04, relwidth=0.15)

    def recreate_download_btn(self):
        self.download_button = Button(self.window, text="Download file", font="Helvetica 10 bold", width=20,
                                      bg="#ABB2B9"
                                      , command=lambda: self.file_download_window())
        self.download_button.place(relx=0.78, rely=0.89, relheight=0.04, relwidth=0.15)

    def is_connected(self):
        if not self.user.connected:
            return False
        return True

    def is_name_free(self):
        if not self.is_available():
            return False
        return True

    def connect_error_msg(self):
        error = Label(self.login, text="----------couldn't connect to server----------", font="Helvetica 12")
        error.place(relheight=0.1, relx=0.37, rely=0.7)

    def is_valid(self, name):
        return len(name) > 1 and not " " in name and name.isalpha() \
               and self.is_english(name)

    def is_english(self, name):
        try:
            name.encode(encoding='utf-8').decode('ascii')
            return True
        except:
            return False
