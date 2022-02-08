class Ports:
    def __init__(self):
        self.lower_bound = 55000
        self.upper_bound = 55015
        self.ports_list: list = [False for _ in range(15)]

    def receive_port(self):
        for port in range(15):
            if not self.ports_list[port]:
                self.ports_list[port] = True
                return port + self.lower_bound
