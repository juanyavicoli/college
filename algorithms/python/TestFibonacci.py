import unittest
from Fibonacci import recursive_fibonacci


class TestFibonacci(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_recursive_fibonacci(self):
        self.assertEqual(recursive_fibonacci(0), 0)
        self.assertEqual(recursive_fibonacci(1), 1)
        self.assertEqual(recursive_fibonacci(2), 1)
        self.assertEqual(recursive_fibonacci(3), 2)
        self.assertEqual(recursive_fibonacci(4), 3)
        self.assertEqual(recursive_fibonacci(5), 5)
        self.assertEqual(recursive_fibonacci(6), 8)
        self.assertEqual(recursive_fibonacci(7), 13)
        self.assertEqual(recursive_fibonacci(8), 21)

        with self.assertRaises(ValueError):
            dummy = recursive_fibonacci(-1)
            dummy = recursive_fibonacci(-10)
            dummy = recursive_fibonacci(-6)
            dummy = recursive_fibonacci(-101010)

