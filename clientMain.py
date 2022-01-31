from HandleClients import HandleClients


if __name__ == '__main__':
    handle = HandleClients()
    handle.connect("ori",123)
    handle.connect("shlomi",456)
    handle.connect("sasi",789)
    handle.get_users()

    # c2 = Client("aaa","345")
    # c2.connect()
    # c2.send_msg_to_server("hey!")
    # c2.disconnect()