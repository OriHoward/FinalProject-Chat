from tkinter import *
from tkinter import messagebox
import ButtonActions
import threads
from Actions import Actions
from SocketHandler import SocketHandler
from User import User


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
        self.label_name = Label(self.login, text="Enter your name: ", font="Helvetica 12")
        self.label_name.place(relheight=0.1, relx=0.03, rely=0.2)
        self.label_address = Label(self.login, text="Enter server ip: ", font="Helvetica 12")
        self.label_address.place(relheight=0.1, relx=0.03, rely=0.5)
        self.window.protocol("WM_DELETE_WINDOW", self.close_chat)
        self.login.protocol("WM_DELETE_WINDOW", self.close_login)
        self.entry_name = Entry(self.login, font="Helvetica 14")
        self.entry_name.place(relwidth=0.4, relheight=0.12, relx=0.35, rely=0.2)
        self.entry_address = Entry(self.login, font="Helvetica 14")
        self.entry_address.place(relwidth=0.4, relheight=0.12, relx=0.35, rely=0.5)
        self.entry_name.focus()
        self.button_action = ButtonActions.ButtonsActions(self)
        self.thread = threads.HandleThreads(self, self.button_action)
        self.btn = Button(self.login, text="CONTINUE", font="Helvetica 14 bold",
                          command=lambda: self.enter_main_window(self.entry_name.get()))
        self.btn.place(relx=0.4, rely=0.75)
        ##todo destroy and terminate program if not logged in
        self.name = None
        self.text_cons = None
        self.new_user: User = None
        self.msg = None

        self.window.mainloop()

    def enter_main_window(self, name):
        if len(name) < 2 or " " in name or not name.isalpha():
            self.error_msg()
            return False
        self.new_user = User(name)
        self.new_user.connect()
        if not self.is_name_free():
            return
        self.login.destroy()
        self.layout(name)
        self.thread.start_receiver()

    # exmaple to use later: for clicked.get() and .pack()
    # new_button = Button(self.window, text="do something", command=self.do_something_accordingly())

    def layout(self, name):
        self.name = name
        self.window.deiconify()
        self.window.title("Welcome to the chat room")
        self.window.resizable(width=True, height=True)
        self.window.configure(width=800, height=550, bg="#17202A")
        self.add_chat_window()
        self.add_buttons()
        self.add_drop_down_menu()
        self.text_cons.config(cursor="arrow")
        self.add_scrollbar()

    def update_chat_display(self, message):
        if self.new_user.is_connected:
            self.text_cons.config(state=NORMAL)
            self.text_cons.insert(END, message + "\n")
            self.text_cons.config(state=DISABLED)
            self.text_cons.see(END)

    def add_buttons(self):
        disconnect_btn = Button(self.window, text="Disconnect", font="Helvetica 10 bold", width=20, bg="#ABB2B9",
                                command=lambda: self.button_action.handle_disconnect())
        disconnect_btn.place(relx=0.78, rely=0.85, relheight=0.04, relwidth=0.15)

        download_button = Button(self.window, text="Download file", font="Helvetica 10 bold", width=20, bg="#ABB2B9"
                                 , command=lambda: self.file_download_window())
        download_button.place(relx=0.78, rely=0.89, relheight=0.04, relwidth=0.15)

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
        # sending the message with ENTER key:
        self.window.bind('<Return>', lambda event: self.send_msg(event, entry_msg.get(), entry_msg))

    def send_msg(self, event, msg, entry_msg):
        self.text_cons.config(state=DISABLED)
        entry_msg.delete(0, END)
        self.thread.start_sender(msg)

    def add_drop_down_menu(self):
        clicked = StringVar()
        clicked.set("Send to")
        drop = OptionMenu(self.window, clicked, "hello")
        drop.configure(bg="#ABB2B9", fg="#EAECEE", font="Helvetica 10 bold", width=10)
        drop.pack()

        drop.place(relx=0.78, rely=0.94, relheight=0.05, relwidth=0.15)
        # todo add to send button option with clicked.get() to know which one was chosen

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
        error = Label(self.login, text="please enter a valid one word name", font="Helvetica 12")
        error.place(relheight=0.1, relx=0.18, rely=0.7)

    def is_available(self):
        if SocketHandler.get_enum(self.new_user.server) == Actions.TRUE.value:
            return True
        self.new_user.disconnect()
        self.taken_name_msg()
        return False

    def taken_name_msg(self):
        error = Label(self.login, text="Name is taken, please try a different name", font="Helvetica 12")
        error.place(relheight=0.1, relx=0.18, rely=0.7)

    def file_download_window(self):
        file_window = Toplevel(self.window)
        file_window.title("Choose your file")
        file_window.configure(width=600, height=550)

    def is_name_free(self):
        if not self.new_user.is_connected:
            return False
        if not self.is_available():
            return False
        return True
