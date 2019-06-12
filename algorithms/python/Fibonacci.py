def recursive_fibonacci(n: int) -> int:
    if n < 0:
        raise ValueError("n argument must be non-negative.")
    elif n < 2:
        return n
    else:
        return recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2)

