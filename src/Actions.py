import enum


class Actions(enum.Enum):
    USER_LIST = b'U'
    DISCONNECT = b'D'
    PRIVATE_MSG = b'P'
    FILE_LIST = b'F'
    MESSAGE_ALL = b'A'
    TRUE = b'Y'
    FALSE = b'N'
