import unittest


proc recursive_fibonacci(n: uint): uint =
    if n < 2:
        result = n
    else:
        result = recursive_fibonacci(n - 1) + recursive_fibonacci(n - 2)

      
suite "recursive_fibonacci":
    test "equals":
        check recursive_fibonacci(0) == 0
        check recursive_fibonacci(1) == 1
        check recursive_fibonacci(2) == 1
        check recursive_fibonacci(3) == 2
        check recursive_fibonacci(4) == 3
        check recursive_fibonacci(5) == 5
        check recursive_fibonacci(6) == 8
        check recursive_fibonacci(7) == 13
        check recursive_fibonacci(8) == 21

