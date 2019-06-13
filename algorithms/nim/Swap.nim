import unittest


proc swap[T](a, b: var T) =
    let c = a
    a = b
    b = c


suite "swap":
    test "int":
        var a: int = 50
        var b: int = 60

        swap(a, b)

        check a == 60
        check b == 50

    test "string":
        var a: string = "hello"
        var b: string = "world"

        swap(a, b)

        check a == "world"
        check b == "hello"

