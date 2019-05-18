import unittest
from Complex import Complex


class TestComplex(unittest.TestCase):

    def setUp(self):
        self.a = Complex(10, 1)
        self.b = Complex(0, -5)
        self.c = Complex(1, 2)
        self.d = Complex(4, 4)
        self.e = Complex(-6, -1)

    def tearDown(self):
        pass

    def test_addition(self):
        self.assertEqual(self.a + self.b, Complex(10, -4))
        self.assertEqual(self.b + self.a, Complex(10, -4))
        self.assertEqual(self.a + self.c, Complex(11, 3))
        self.assertEqual(self.c + self.a, Complex(11, 3))
        self.assertEqual(self.c + self.d, Complex(5, 6))
        self.assertEqual(self.d + self.e, Complex(-2, 3))

    def test_subtraction(self):
        self.assertEqual(self.a - self.b, Complex(10, 6))
        self.assertEqual(self.b - self.a, Complex(-10, -6))
        self.assertEqual(self.c - self.b, Complex(1, 7))
        self.assertEqual(self.d - self.c, Complex(3, 2))
        self.assertEqual(self.e - self.d, Complex(-10, -5))
    
    def test_scalar_multiplication(self):
        pass
        
    def test_complex_multiplication(self):
        pass
        
    def test_equality(self):
        pass
