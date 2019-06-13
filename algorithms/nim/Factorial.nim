import unittest


proc recursive_factorial(n: uint): uint =
    if n == 0:
        result = 1
    else:
        result = n * recursive_factorial(n - 1)


suite "recursive_factorial":
    test "equals":
        check recursive_factorial(0) == 1
        check recursive_factorial(1) == 1
        check recursive_factorial(2) == 2
        check recursive_factorial(3) == 6
        check recursive_factorial(4) == 24
        check recursive_factorial(5) == 120
        check recursive_factorial(6) == 720

