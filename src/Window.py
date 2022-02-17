import math


class Window:
    def __init__(self, sequence_number_bits, window_size=None):
        self.sequence_number_bits = sequence_number_bits
        if window_size is None:
            max_size = int(math.pow(2, self.sequence_number_bits - 1))
            self.window_size = max_size
