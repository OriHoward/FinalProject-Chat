class Ports:
    def __init__(self, lower_bound: int, upper_bound: int):
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
        self.ports: list = [False for _ in range(self.upper_bound - self.lower_bound)]

    def check_for_ports(self):
        for port in range(len(self.ports)):
            if not self.ports[port]:
                self.ports[port] = True
                return port + self.lower_bound
