from User import User

if __name__ == '__main__':
    ori = User("ori")
    shlomi = User("shlomi")
    ori.connect()
    shlomi.connect()
    shlomi.get_users()
    ori.send_private_msg("hello","shlomi")


    ori.disconnect()
    shlomi.disconnect()
