import multiprocessing

from ChatGUI import ChatGUI
from User import User

if __name__ == '__main__':
    try:
        num_of_clients = int(input("please enter number of clients (1 to 15): "))
        for i in range(num_of_clients):
            new_client = User()
            process = multiprocessing.Process(target=ChatGUI, args=(new_client,))
            process.start()
    except:
        print("enter numbers only")
