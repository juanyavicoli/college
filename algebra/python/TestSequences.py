import unittest
from Sequences import Sequences

class TestSequences(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_fibonacci(self):
        self.assertEqual(Sequences.fibonacci(0), 0)
        self.assertEqual(Sequences.fibonacci(1), 1)
        self.assertEqual(Sequences.fibonacci(2), 1)
        self.assertEqual(Sequences.fibonacci(3), 2)
        self.assertEqual(Sequences.fibonacci(4), 3)
        self.assertEqual(Sequences.fibonacci(5), 5)
        self.assertEqual(Sequences.fibonacci(6), 8)
        self.assertEqual(Sequences.fibonacci(7), 13)
        self.assertEqual(Sequences.fibonacci(8), 21)

        with self.assertRaises(ValueError):
            dummy = Sequences.fibonacci(-1)
            dummy = Sequences.fibonacci(-10)
            dummy = Sequences.fibonacci(-6)
            dummy = Sequences.fibonacci(-101010)

