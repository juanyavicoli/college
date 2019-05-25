class Sequences:

    def __init__(self):
        pass

    @staticmethod
    def fibonacci(n: int):
        if n < 0:
            raise ValueError("N must be positive integer.")

        if n == 0:
            return 0
        elif n == 1:
            return 1
        else:
            return Sequences.fibonacci(n - 1) + Sequences.fibonacci(n - 2)

