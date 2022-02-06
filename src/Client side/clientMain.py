import threading

from ChatGUI import ChatGUI

if __name__ == '__main__':
    for i in range(3):
        thread = threading.Thread(target=ChatGUI)
        thread.start()
