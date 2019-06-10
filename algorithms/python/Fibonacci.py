def recursive_fibonacci(n: int) -> int:
    if n < 0:
        raise ValueError("N must be positive integer.")

    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2)

