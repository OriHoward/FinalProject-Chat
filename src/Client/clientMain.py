import multiprocessing

from ChatGUI import ChatGUI

if __name__ == '__main__':
    try:
        num_of_clients = int(input("please enter number of clients (1 to 15): "))
        for i in range(num_of_clients):
            process = multiprocessing.Process(target=ChatGUI)
            process.start()
    except:
        print("enter numbers only")
