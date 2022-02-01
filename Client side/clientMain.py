from User import User

if __name__ == '__main__':
    ori = User("ori")
    shlomi = User("shlomi")
    ori.connect()
    shlomi.connect()
    ori.get_users()
    ori.disconnect()
    shlomi.get_users()
    shlomi.disconnect()

    # c2 = Client("aaa","345")
    # c2.connect()
    # c2.send_msg_to_server("hey!")
    # c2.disconnect()