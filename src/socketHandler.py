FORMAT = 'utf-8'


class socketHandler:

    @staticmethod
    def get_msg(conn):
        try:
            msg_length_bytes = bytearray(conn.recv(4))
            msg_len = int.from_bytes(msg_length_bytes, "little", signed=False)
            if msg_len:
                return conn.recv(msg_len).decode(FORMAT)
        except:
            conn.close()

    @staticmethod
    def send_msg(msg, conn):
        try:
            if msg is not None:
                msg_length = len(msg)
                msg_len_bytes = msg_length.to_bytes(4, 'little')
                conn.send(msg_len_bytes)
                msg = msg.encode(FORMAT)
                conn.send(msg)
        except:
            conn.close()
            print("An error occurred")
