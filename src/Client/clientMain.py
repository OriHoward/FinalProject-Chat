import multiprocessing

from ChatGUI import ChatGUI

if __name__ == '__main__':
    num_of_clients = int(input("please enter number of clients: "))
    for i in range(num_of_clients):
        process = multiprocessing.Process(target=ChatGUI)
        process.start()