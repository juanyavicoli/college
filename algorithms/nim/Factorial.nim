import unittest


proc recursive_factorial(n: uint): uint =
    if n == 0:
        result = 1
    else:
        result = n * recursive_factorial(n - 1)


proc while_loop_factorial(n: uint): uint =
    result = 1
    var i: uint = 1

    while i < n:
        result *= (i + 1)
        inc(i)


proc for_loop_factorial(n: uint): uint =
    result = 1

    for i in 1..n:
      result *= uint(i)


suite "recursive_factorial":
    test "equals":
        check recursive_factorial(0) == 1
        check recursive_factorial(1) == 1
        check recursive_factorial(2) == 2
        check recursive_factorial(3) == 6
        check recursive_factorial(4) == 24
        check recursive_factorial(5) == 120
        check recursive_factorial(6) == 720


suite "while_loop_factorial":
    test "equals":
        check while_loop_factorial(0) == 1
        check while_loop_factorial(1) == 1
        check while_loop_factorial(2) == 2
        check while_loop_factorial(3) == 6
        check while_loop_factorial(4) == 24
        check while_loop_factorial(5) == 120
        check while_loop_factorial(6) == 720


suite "for_loop_factorial":
    test "equals":
        check for_loop_factorial(0) == 1
        check for_loop_factorial(1) == 1
        check for_loop_factorial(2) == 2
        check for_loop_factorial(3) == 6
        check for_loop_factorial(4) == 24
        check for_loop_factorial(5) == 120
        check for_loop_factorial(6) == 720

